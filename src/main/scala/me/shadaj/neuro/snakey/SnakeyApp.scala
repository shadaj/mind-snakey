package me.shadaj.neuro.snakey

import scala.swing.BoxPanel
import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.SimpleSwingApplication
import scala.swing.event.KeyPressed

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala

object SnakeyApp extends SimpleSwingApplication {
  private var mindControl = false

  val system = ActorSystem("SnakeySystem")

  private val snakeyActor = system.actorOf(Props(new SnakeyActor(panel.levelChooser.selection.item)), "snakeyActor")

  var screen = new SnakeyScreen(mindControl)

  val panel = new SnakeyPanel()

  var dead = false

  val frameContents = new BoxPanel(Orientation.Vertical) {
    contents += screen
    contents += panel

    listenTo(this.keys)
    reactions += {
      case KeyPressed(_, k, _, _) =>
        screen.processKey(k)
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
    if (panel != null) {
      panel.setInfo(text)
    }
  }

  def die {
    panel.die
    dead = true
  }

  def eatFruit {
    fruitsEaten += 1
    panel.fruits.text = "Fruits eaten: " + fruitsEaten
  }

  def start {
    if (dead) {
      reset
    } else {
      disableChoosers
      snakeyActor ! StartGame
    }
  }

  def reset {
    if (mindControl) {
      screen.neuroThingy.waitingForStop = true
    }
    snakeyActor ! Reset
    panel.fruits.text = "Fruits eaten: 0"
    fruitsEaten = 0
    frameContents.contents.clear
    screen.timer.stop()
    screen = new SnakeyScreen(mindControl)
    frameContents.contents += screen
    frameContents.contents += panel
    panel.startButton.enabled = true
    panel.revalidate
    panel.startButton.text = "Start"
    dead = false
  }

  def enableChoosers {
    if (panel != null) {
      panel.enable
    }
  }

  def disableChoosers {
    if (panel != null) {
      panel.disable
    }
  }
}