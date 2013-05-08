package me.shadaj.neuro.snakey

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import scala.swing.BoxPanel
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import javax.swing.Timer
import scala.swing._
import scala.swing.event.KeyEvent
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import scala.swing.event.Key
import javax.swing.border.Border

object SnakeyLauncher extends Snakey

class Snakey extends SimpleSwingApplication {
  val game = new SnakeyScreen(this)
  val panel = new SnakeyPanel(this)

  def top = new MainFrame {
    title = "Snakey"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += game
      contents += panel

      listenTo(this.keys)
      reactions += {
        case KeyPressed(_, k, _, _) =>
          game.processKey(k)
      }

      focusable = true
    }

    size = new Dimension(1000, 1000)

    panel.maximumSize = new Dimension(1000, 50)
  }
  
  var fruitsEaten = 0

  def die {
    panel.die
  }
  
  def eatFruit {
    fruitsEaten += 1
    panel.fruits.text = "Fruits eaten: " + fruitsEaten
  }
}

class SnakeyScreen(host: Snakey, mindControl: Boolean = true) extends Panel {
  var dead = false

  val FRAME_RATE = 100
  val GRID_SIZE = 50

  val listener = new SnakeyKeyListener(this)

  def screenSize = size

  def screenWidth = size.width
  def screenHeight = size.height

  def boxWidth = screenWidth / GRID_SIZE
  def boxHeight = screenHeight / GRID_SIZE

  var snake = new Snake(Seq(new Part(GRID_SIZE / 2, GRID_SIZE / 2, Up)))
  var fruit = Fruit(GRID_SIZE, GRID_SIZE)

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

  override def paint(g: Graphics2D) {
    g.clearRect(0, 0, screenWidth, screenHeight)
    drawSnake(g)
    drawFruit(g)
  }

  class FrameRateUpdate extends ActionListener {
    def actionPerformed(event: ActionEvent) {
      if (!dead) {
        val eatingSelf = snake.parts.tail.exists {
          case Part(x, y, _) =>
            snake.parts.head.x == x && snake.parts.head.y == y
        }

        if (eatingSelf) {
          host.die
          dead = true
        }

        if (snake.parts.head.x == fruit.x && snake.parts.head.y == fruit.y) {
          snake = snake.grow
          fruit = Fruit(GRID_SIZE, GRID_SIZE)
          host.eatFruit
        }

        snake = snake.move

        repaint()
      }
    }
  }

  val timer = new Timer(FRAME_RATE, new FrameRateUpdate)

  timer.start()
}

class SnakeyPanel(host: Snakey) extends GridPanel(1, 3) {
  def die {
    mainInfo.text = "You died :( I told you not to eat yourself!"
  }

  border = new javax.swing.border.EtchedBorder
  
  val mainInfo = new Label("You're doing great!")
  contents += mainInfo
  
  val fruits = new Label("Fruits eaten: None")
  contents += fruits
}