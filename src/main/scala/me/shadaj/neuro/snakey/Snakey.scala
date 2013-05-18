package me.shadaj.neuro.snakey

import java.awt.Color
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Dimension
import scala.swing.Graphics2D
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.util.Success
import scala.util.Try
import javax.swing.Timer
import me.shadaj.neuro.thinkgear.Blink
import me.shadaj.neuro.thinkgear.NeuroIterator
import me.shadaj.neuro.thinkgear.PoorSignalLevel
import scala.util.Failure
import scala.swing.event.ButtonClicked
import scala.swing.ComboBox
import javax.imageio.ImageIO
import java.io.File
import scala.annotation.tailrec

object SnakeyLauncher extends Snakey

class Snakey extends SimpleSwingApplication {
  var game = new SnakeyScreen(this, 0, true)
  val panel = new SnakeyPanel(this)

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
      case e =>
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
      reset
    }

  }

  def reset {
    fruitsEaten = 0
    frameContents.contents.clear
    game = new SnakeyScreen(this, panel.levelChooser.selection.item, true)
    frameContents.contents += game
    frameContents.contents += panel
    panel.startButton.enabled = true
    panel.revalidate
    start
  }
}

class SnakeyScreen(host: Snakey, var LEVEL: Int, mindControl: Boolean = true) extends Panel {
  var running = false
  var dead = false
  var waitingForBlinkConfirmation = false

  def start {
    running = true
  }

  val FRAME_RATE = 100
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
  val connecting = List(ImageIO.read(new File("connecting1.png")), ImageIO.read(new File("connecting2.png")), ImageIO.read(new File("connecting3.png")))

  var imageToShow = nosignalImage
  var currentConnecting = 0

  override def paint(g: Graphics2D) {
    g.clearRect(0, 0, screenWidth, screenHeight)
    drawSnake(g)
    drawFruit(g)
    drawBadBlocks(g)
    if (mindControl) {
      g.drawImage(imageToShow, boxWidth * (GRID_SIZE - 3), boxHeight, boxWidth * 2, boxHeight * 2, null)
    }
    //    g.drawImage(workingImage, 0, 0, null)
  }

  class FrameRateUpdate extends ActionListener {
    def actionPerformed(event: ActionEvent) {
      if (!dead && running) {
        if (mindControl && timeSinceBlink > MAX_DOUBLEBLINK && lastBlink != 0) {
          val newDirection = snake.direction.turnRight
          snake = snake.turn(newDirection)
          lastBlink = 0
          waitingForBlinkConfirmation = false
        }

        val eatingSelf = snake.parts.tail.exists {
          case Part(x, y, _) =>
            snake.parts.head.x == x && snake.parts.head.y == y
        }

        val coordinates = (snake.parts.head.x, snake.parts.head.y)

        if (eatingSelf || badBlocks.contains(coordinates)) {
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

  @tailrec
  private def connect: Try[NeuroIterator] = {
    val iterator = Try(new NeuroIterator)
    iterator match {
      case Success(v) => {
        host.setInfo("Succesfully connected!")
        Success(v)
      }

      case Failure(e) => {
        host.setInfo("Please check that ThinkGearConnector is on.")
        imageToShow = nosignalImage
        connect
      }
    }
  }


  class NeuroReader extends Thread {
    override def run {
      val in = connect.get
      in.foreach { d =>
        if (!dead && running) {
          d match {
            case Blink(power: Int) => {
              if (power >= MIN_BLINK) {
                if (timeSinceBlink <= MAX_DOUBLEBLINK) {
                  snake = snake.turn(snake.direction.turnLeft)
                  lastBlink = 0
                  waitingForBlinkConfirmation = false
                } else {
                  lastBlink = System.currentTimeMillis
                  waitingForBlinkConfirmation = true
                }
              }
            }
            case PoorSignalLevel(level: Int) => {
              println(level)
              host.badSignal
              imageToShow = connecting(currentConnecting)
              currentConnecting = if (currentConnecting == 2) 0 else currentConnecting + 1
            }
            case _ => imageToShow = connectedImage
          }
        } else {
          d match {
            case PoorSignalLevel(level: Int) => {
              host.badSignal
              imageToShow = connecting(currentConnecting)
              currentConnecting = if (currentConnecting == 2) 0 else currentConnecting + 1
            }
            case _ => imageToShow = connectedImage
          }
        }
      }
    }
  }

  val timer = new Timer(FRAME_RATE, new FrameRateUpdate)

  timer.start()

  if (mindControl) {
    val reader = new NeuroReader
    reader.start()
  }
}

class SnakeyPanel(host: Snakey) extends GridPanel(1, 3) {
  val mainInfo = new Label("You're doing great!")
  contents += mainInfo

  def setInfo(text: String) { mainInfo.text = text }

  def die {
    mainInfo.text = "You died :( I told you not to eat yourself!"
    startButton.enabled = true
    levelChooser.enabled = true
  }

  def badSignal {
    mainInfo.text = "Looks like your headset isn't on!"
  }

  border = new javax.swing.border.EtchedBorder

  val fruits = new Label("Fruits eaten: None")
  contents += fruits

  val startButton = new Button("Start")
  contents += startButton

  val levelChooser = new ComboBox((0 to 20))
  contents += levelChooser
  levelChooser.focusable = false

  listenTo(startButton)

  startButton.reactions += {
    case ButtonClicked(b) =>
      host.start
  }
}