package me.shadaj.neuro.snakey

import java.awt.Color
import java.awt.event._

import javax.swing.Timer

import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

import scala.swing._
import scala.swing.event._

import scala.util.Success
import scala.util.Failure
import scala.util.Try

import scala.annotation.tailrec

import me.shadaj.neuro.thinkgear._

object SnakeyLauncher extends Snakey

class Snakey extends SimpleSwingApplication {
  var game = new SnakeyScreen(this, 0, true)
  val panel = new SnakeyPanel(this)

  var mindControl = true

  val frameContents = new BoxPanel(Orientation.Vertical) {
    contents += game
    contents += panel

    listenTo(this.keys)
    reactions += {
      case KeyPressed(_, k, _, _) =>
        game.processKey(k)
    }

    focusable = true
  }

  def top = new MainFrame {
    title = "Snakey"
    contents = frameContents

    size = new Dimension(1000, 1000)

    panel.maximumSize = new Dimension(1000, 50)
  }

  var fruitsEaten = 0

  def setInfo(text: String) {
    try panel.setInfo(text) catch {
      case e: Throwable =>
    }
  }

  def die {
    panel.die
  }

  def badSignal {
    if (!game.dead) {
      panel.badSignal
    }
  }

  def eatFruit {
    fruitsEaten += 1
    panel.fruits.text = "Fruits eaten: " + fruitsEaten
  }

  def start {
    if (!game.dead) {
      game.LEVEL = panel.levelChooser.selection.item
      game.start
      panel.startButton.enabled = false
      panel.levelChooser.enabled = false
    } else {
      game.end
      panel.fruits.text = "Fruits eaten: 0"
      reset
    }
  }

  def reset {
    fruitsEaten = 0
    frameContents.contents.clear
    game = new SnakeyScreen(this, panel.levelChooser.selection.item, mindControl)
    frameContents.contents += game
    frameContents.contents += panel
    panel.startButton.enabled = true
    panel.revalidate
  }

  def disableChoosers {
    try {
      panel.startButton.enabled = false
      panel.levelChooser.enabled = false
    } catch {
      case _ =>
    }
  }

  def enableChoosers {
    try {
      panel.startButton.enabled = true
      panel.levelChooser.enabled = true
    } catch {
      case _ =>
    }
  }
}

class SnakeyScreen(host: Snakey, var LEVEL: Int, mindControl: Boolean = true) extends Panel {
  var running = false
  var dead = false
  def waitingForBlinkConfirmation = lastBlink != 0
  var waitingForStop = false

  def start {
    running = true
  }

  def end {
    waitingForStop = true
  }

  val FRAME_RATE = 150
  val GRID_SIZE = 50

  //Neuro Settings
  val MIN_BLINK = 10
  val MAX_DOUBLEBLINK = 750 //In milliseconds

  val listener = new SnakeyKeyListener(this)

  def screenSize = size

  def screenWidth = size.width
  def screenHeight = size.height

  def boxWidth = screenWidth / GRID_SIZE
  def boxHeight = screenHeight / GRID_SIZE

  val middleOfGrid = GRID_SIZE / 2

  val SPACE_FROM_CENTER = 5

  val badBlocks = (0 until GRID_SIZE).flatMap(x => (0 until GRID_SIZE).map(y => (x, y))).filter {
    case (x, y) =>
      (math.random * 10 <= LEVEL / 20D ||
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
    snake.parts.foreach {
      case Part(x, y, _) =>
        g.setPaint(Color.green)
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
      currentImage = if (currentImage == 2) 0 else currentImage + 1
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

  class FrameRateUpdate extends ActionListener {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      if (!dead && running) {
        if (mindControl && timeSinceBlink > MAX_DOUBLEBLINK && lastBlink != 0) {
          val newDirection = snake.direction.right
          snake = snake.turn(newDirection)
          lastBlink = 0
        }

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

  val timer = new Timer(FRAME_RATE, new FrameRateUpdate)

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

  if (mindControl) {
    object NeuroReader extends Thread {
      def read {
        while (!waitingForStop) {
          val d = in.next
          if (!dead && running) {
            d match {
              case Blink(power: Int) => {
                if (power >= MIN_BLINK) {
                  if (timeSinceBlink <= MAX_DOUBLEBLINK) {
                    snake = snake.turn(snake.direction.left)
                    lastBlink = 0
                  } else {
                    lastBlink = System.currentTimeMillis
                  }
                }
              }

              case PoorSignalLevel(level: Int) => {
                host.badSignal
                imageToShow = ConnectingImageIterator.next
              }

              case EEG(_, _, PoorSignalLevel(level)) => {
                if (level > 0) {
                  imageToShow = ConnectingImageIterator.next
                } else {
                  imageToShow = connectedImage
                }
              }

              case _ =>
            }
          } else {
            d match {
              case PoorSignalLevel(level: Int) => {
                host.badSignal
                host.panel.startButton.enabled = false
                imageToShow = ConnectingImageIterator.next
              }

              case EEG(_, _, PoorSignalLevel(level)) => {
                if (level > 0) {
                  if (!running) {
                    host.panel.startButton.enabled = false
                  }
                  imageToShow = ConnectingImageIterator.next
                } else {
                  host.panel.startButton.enabled = true
                  imageToShow = connectedImage
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