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
import akka.actor.Props
import akka.actor._

case class DoneDrawing(screenActor: ActorRef)

class SnakeyScreen(val level: Int, mindControl: Boolean = true) extends Panel {
  val host = SnakeyApp

  val snakeyActor = host.system.actorFor("/user/snakeyActor")

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

  def processKey(k: Key.Value) = {
    snakeyActor ! (k match {
      case Key.Up | Key.W => Up
      case Key.Left | Key.A => Left
      case Key.Down | Key.S => Down
      case Key.Right | Key.D => Right
      case _ =>
    })
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

  var imageToDraw: BufferedImage = null

  override def paint(g: Graphics2D) {
    g.drawImage(imageToDraw, null, 0, 0)
  }

  val screenActor = host.system.actorOf(Props(ScreenActor))
  
  object FrameRateUpdate extends ActionListener {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      snakeyActor ! Tick
      screenActor ! Draw
    }
  }

  val timer = new Timer(FRAME_RATE, FrameRateUpdate)

  timer.start()

  def processPoorSignal() {
    snakeyActor ! BadSignal
    host.panel.startButton.enabled = false
    imageToShow = ConnectingImageIterator.next
    snakeyActor ! PauseGame
  }

  var neuroThingy: NeuroThingy = null

  if (mindControl) {
    neuroThingy = new NeuroThingy
    neuroThingy.start
  }

  object ScreenActor extends Actor {
    def receive = {
      case Draw => {
        imageToDraw = new BufferedImage(screenWidth, screenHeight, BufferedImage.TYPE_INT_RGB)
        val graphics = imageToDraw.getGraphics().asInstanceOf[Graphics2D]
        snakeyActor ! Draw(graphics, self)
        if (mindControl) {
          graphics.drawImage(imageToShow, boxWidth * (GRID_SIZE - 3), boxHeight, boxWidth * 2, boxHeight * 2, null)
        }
      }

      case DoneDrawing(_) => {
        repaint
      }
    }
  }
}