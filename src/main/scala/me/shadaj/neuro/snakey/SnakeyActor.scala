package me.shadaj.neuro.snakey

import akka.actor.Actor
import me.shadaj.neuro.thinkgear.NeuroData
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import me.shadaj.neuro.thinkgear.NeuroIterator
import me.shadaj.neuro.thinkgear.Blink
import me.shadaj.neuro.thinkgear.PoorSignalLevel
import me.shadaj.neuro.thinkgear.EEG
import akka.actor.ActorSystem
import akka.actor.Props
import java.awt.Graphics2D
import java.awt.Color

case object Tick

case object StartGame
case object StopGame
case object PauseGame
case object UnpauseGame

case object FruitEaten

class SnakeyActor(level: Int) extends Actor {
  val host = SnakeyApp
  def screen = SnakeyApp.screen

  val MIN_BLINK = 10
  val MAX_DOUBLEBLINK = 750 //In milliseconds

  val GRID_SIZE = screen.GRID_SIZE

  val middleOfGrid = GRID_SIZE / 2

  val SPACE_FROM_CENTER = 5

  var started = false
  var paused = false

  var lastBlink = 0L

  def timeSinceBlink = System.currentTimeMillis - lastBlink

  def gameRunning = started && !paused

  def waitingForBlink = !(lastBlink == 0)

  val snakeActor = context.actorOf(Props[SnakeActor])

  val badBlocks = (0 until GRID_SIZE).flatMap(x => (0 until GRID_SIZE).map(y => (x, y))).filter {
    case (x, y) =>
      (math.random * 10 <= level / 20D ||
        (x == 0 || x == GRID_SIZE - 1 || y == 0 || y == GRID_SIZE - 1)) && ((math.abs(middleOfGrid - x) >= SPACE_FROM_CENTER || math.abs(middleOfGrid - y) >= SPACE_FROM_CENTER))
  }

  var fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks, screen.boxWidth, screen.boxHeight)

  def drawBadBlocks(g: Graphics2D) {
    g.setPaint(Color.white)
    badBlocks.foreach {
      case (x, y) =>
        g.fillRect(x * screen.boxWidth, y * screen.boxHeight, screen.boxWidth, screen.boxHeight)
    }
  }
  def receive = {
    case Tick if gameRunning => {
      snakeActor ! CheckEating(fruit, self)
      if (!waitingForBlink) {
        snakeActor ! Move
      }
      if (timeSinceBlink > MAX_DOUBLEBLINK && lastBlink != 0) {
        snakeActor ! TurnRight
        lastBlink = 0
      }
      snakeActor ! Tick
      snakeActor ! CheckBadBlocks(badBlocks, self)
    }

    case Blink(power: Int) if power >= screen.MIN_BLINK && gameRunning => {
      if (timeSinceBlink <= MAX_DOUBLEBLINK) {
        snakeActor ! TurnLeft
        lastBlink = 0
      } else {
        lastBlink = System.currentTimeMillis
      }
    }

    case PoorSignalLevel(level: Int) => {
      screen.processPoorSignal()
    }

    case EEG(_, _, PoorSignalLevel(level)) if gameRunning => {
      if (level > 0) {
        screen.processPoorSignal()
      } else {
        screen.imageToShow = screen.connectedImage
      }
    }

    case EEG(_, _, PoorSignalLevel(level)) => {
      if (level > 0) {
        screen.processPoorSignal()
      } else {
        host.panel.startButton.enabled = true
        screen.imageToShow = screen.connectedImage
        paused = false
        host.setInfo("Connected!")
      }
    }

    case BadSignal => {
      snakeActor ! BadSignal
    }

    case d: Direction => {
      snakeActor ! d
    }

    case Draw(g, _) => {
      drawBadBlocks(g)
      fruit.draw(g)
      snakeActor ! Draw(g, self)
    }

    case DoneDrawing(_) => {
      screen.screenActor ! DoneDrawing(self)
    }

    case StartGame => {
      host.disableChoosers
      started = true
    }

    case StopGame => {
      host.enableChoosers
      started = false
    }

    case PauseGame => {
      paused = true
    }

    case UnpauseGame => {
      paused = false
    }

    case FruitEaten => fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks, screen.boxWidth, screen.boxHeight)

    case Eating => self ! FruitEaten

    case NotEating =>

    case Reset => snakeActor ! Reset
  }
}

class NeuroThingy extends Thread {
  val host = SnakeyApp
  def screen = SnakeyApp.screen

  var waitingForStop = false

  val snakeyActor = host.system.actorFor("/user/snakeyActor")

  lazy val in = connect.get

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
        screen.imageToShow = screen.nosignalImage
        connect
      }
    }
  }

  override def run {
    while (!waitingForStop) {
      snakeyActor ! in.next
    }
    in.neuroSocket.close()
  }
}