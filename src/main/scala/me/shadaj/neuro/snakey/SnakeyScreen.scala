package me.shadaj.neuro.snakey

import me.shadaj.neuro.thinkgear.Blink
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Graphics2D
import me.shadaj.neuro.thinkgear.EEG
import me.shadaj.neuro.thinkgear.PoorSignalLevel
import java.awt.event.ActionListener
import scala.swing.Panel
import scala.util.Failure
import javax.swing.Timer
import scala.util.Try
import scala.util.Success
import scala.swing.event.Key
import me.shadaj.neuro.thinkgear.NeuroIterator
import java.awt.Color
import java.io.File

class SnakeyScreen(host: SnakeyApp, val level: Int, mindControl: Boolean = true) extends Panel {
  var started = false
  var dead = false
  def waitingForBlinkConfirmation = lastBlink != 0
  var waitingForStop = false
  var paused = false

  def start {
    started = true
  }

  def end {
    waitingForStop = true
  }

  val FRAME_RATE = 150
  val GRID_SIZE = 50

  //Neuro Settings
  val MIN_BLINK = 10
  val MAX_DOUBLEBLINK = 750 //In milliseconds

  def screenSize = size

  def screenWidth = size.width
  def screenHeight = size.height

  def boxWidth = screenWidth / GRID_SIZE
  def boxHeight = screenHeight / GRID_SIZE

  val middleOfGrid = GRID_SIZE / 2

  val SPACE_FROM_CENTER = 5

  val badBlocks = (0 until GRID_SIZE).flatMap(x => (0 until GRID_SIZE).map(y => (x, y))).filter {
    case (x, y) =>
      (math.random * 10 <= level / 20D ||
        (x == 0 || x == GRID_SIZE - 1 || y == 0 || y == GRID_SIZE - 1)) && ((math.abs(middleOfGrid - x) >= SPACE_FROM_CENTER || math.abs(middleOfGrid - y) >= SPACE_FROM_CENTER))
  }

  var snake = new Snake(Seq(new Part(middleOfGrid, middleOfGrid, Up)))
  var fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks)

  def processKey(k: Key.Value) = {
    snake = snake.turn(k match {
      case Key.Up | Key.W => Up
      case Key.Left | Key.A => Left
      case Key.Down | Key.S => Down
      case Key.Right | Key.D => Right
      case _ => snake.direction
    })
  }

  def drawSnake(g: Graphics2D) {
    g.setPaint(Color.green)
    snake.parts.foreach {
      case Part(x, y, _) =>
        g.fillRect(x * boxWidth, y * boxHeight, boxWidth, boxHeight)
    }
  }

  def drawFruit(g: Graphics2D) {
    g.setPaint(Color.red)
    g.fillOval(fruit.x * boxWidth, fruit.y * boxHeight, boxWidth, boxHeight)
  }

  def drawBadBlocks(g: Graphics2D) {
    g.setPaint(Color.black)
    badBlocks.foreach {
      case (x, y) =>
        g.fillRect(x * boxWidth, y * boxHeight, boxWidth, boxHeight)
    }
  }

  val connectedImage = ImageIO.read(new File("connected.png"))
  val nosignalImage = ImageIO.read(new File("nosignal.png"))
  val connectingImages = List(ImageIO.read(new File("connecting1.png")), ImageIO.read(new File("connecting2.png")), ImageIO.read(new File("connecting3.png")))

  object ConnectingImageIterator extends Iterator[BufferedImage] {
    var currentImage = 2

    def hasNext = true
    def next = {
      currentImage = (currentImage + 1) % connectingImages.size
      connectingImages(currentImage)
    }
  }

  var imageToShow = nosignalImage

  override def paint(g: Graphics2D) {
    g.clearRect(0, 0, screenWidth, screenHeight)
    drawSnake(g)
    drawFruit(g)
    drawBadBlocks(g)
    if (mindControl) {
      g.drawImage(imageToShow, boxWidth * (GRID_SIZE - 3), boxHeight, boxWidth * 2, boxHeight * 2, null)
    }
  }

  object FrameRateUpdate extends ActionListener {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      if (!dead && started && !paused) {
        println("paused: " + paused)
        val eatingSelf = snake.parts.tail.exists {
          case Part(x, y, _) =>
            snake.parts.head.x == x && snake.parts.head.y == y
        }

        val coordinates = (snake.parts.head.x, snake.parts.head.y)

        if (badBlocks.contains(coordinates)) {
          snake = (new Snake(snake.parts.reverse.map(p => new Part(p.x, p.y, p.direction.opposite)))).move
        } else if (eatingSelf) {
          host.die
          dead = true
        } else {
          if (snake.parts.head.x == fruit.x && snake.parts.head.y == fruit.y) {
            snake = snake.grow
            fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks)
            host.eatFruit
          }

          if (!waitingForBlinkConfirmation) {
            snake = snake.move
          }
        }
      }

      repaint()
    }
  }

  var lastBlink = 0L

  def timeSinceBlink = System.currentTimeMillis - lastBlink

  val timer = new Timer(FRAME_RATE, FrameRateUpdate)

  timer.start()

  private def connect: Try[NeuroIterator] = {
    val iterator = Try(new NeuroIterator)
    iterator match {
      case Success(v) => {
        host.setInfo("Succesfully connected!")
        host.enableChoosers
        Success(v)
      }

      case Failure(e) => {
        host.setInfo("Please check that ThinkGearConnector is on.")
        imageToShow = nosignalImage
        connect
      }
    }
  }

  lazy val in = connect.get

  def processPoorSignal() {
    host.badSignal
    host.panel.startButton.enabled = false
    imageToShow = ConnectingImageIterator.next
    paused = true
  }

  if (mindControl) {
    object NeuroReader extends Thread {
      def read {
        while (!waitingForStop) {
          val d = in.next
          if (!dead && started && !paused) {
            d match {
              case Blink(power: Int) if power >= MIN_BLINK => {
                if (timeSinceBlink <= MAX_DOUBLEBLINK) {
                  snake = snake.turn(snake.direction.left)
                  lastBlink = 0
                } else {
                  lastBlink = System.currentTimeMillis
                }
              }

              case EEG(_, _, PoorSignalLevel(level)) => {
                if (level > 0) {
                  processPoorSignal()
                } else {
                  imageToShow = connectedImage
                }

                if (timeSinceBlink > MAX_DOUBLEBLINK && lastBlink != 0) {
                  val newDirection = snake.direction.right
                  snake = snake.turn(newDirection)
                  lastBlink = 0
                }
              }

              case PoorSignalLevel(level: Int) => {
                processPoorSignal()
              }

              case _ =>
            }
          } else {
            d match {
              case PoorSignalLevel(level: Int) => {
                processPoorSignal()
              }

              case EEG(_, _, PoorSignalLevel(level)) => {
                if (level > 0) {
                  processPoorSignal()
                } else {
                  host.panel.startButton.enabled = true
                  imageToShow = connectedImage
                  paused = false
                }
              }

              case _ =>
            }
          }
        }

        in.neuroSocket.close()
      }

      override def run {
        read
      }
    }

    NeuroReader.start
  }
}