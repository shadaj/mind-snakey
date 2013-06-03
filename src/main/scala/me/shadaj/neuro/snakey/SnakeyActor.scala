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

class SnakeyActor(level: => Int) extends Actor {
  val host = SnakeyApp
  def screen = SnakeyApp.screen

  val MIN_BLINK_POWER = 10
  val DOUBLEBLINK = 750 //In milliseconds

  val GRID_SIZE = screen.GRID_SIZE

  val middleOfGrid = GRID_SIZE / 2

  val SPACE_FROM_CENTER = 5

  var paused = false

  var lastBlink = 0L

  def timeSinceBlink = System.currentTimeMillis - lastBlink

  def waitingForBlink = !(lastBlink == 0)

  val snakeActor = context.actorOf(Props[SnakeActor])

  def newBadBlocks = {
    (0 until GRID_SIZE).flatMap(x => (0 until GRID_SIZE).map(y => (x, y))).filter {
      case (x, y) =>
        (math.random * 10 <= level / 20D ||
          (x == 0 || x == GRID_SIZE - 1 || y == 0 || y == GRID_SIZE - 1)) && ((math.abs(middleOfGrid - x) >= SPACE_FROM_CENTER || math.abs(middleOfGrid - y) >= SPACE_FROM_CENTER))
    }
  }
  
  var badBlocks = newBadBlocks

  var fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks, screen.boxWidth, screen.boxHeight)

  def drawBadBlocks(g: Graphics2D) {
    g.setPaint(Color.white)
    badBlocks.foreach {
      case (x, y) =>
        g.fillRect(x * screen.boxWidth, y * screen.boxHeight, screen.boxWidth, screen.boxHeight)
    }
  }

  def running: Receive = {
    case Tick => {
      snakeActor ! CheckEating(fruit, self)
      if (!waitingForBlink) {
        snakeActor ! Move
      }
      if (timeSinceBlink > DOUBLEBLINK && lastBlink != 0) {
        snakeActor ! TurnRight
        lastBlink = 0
      }
      snakeActor ! Tick
      snakeActor ! CheckBadBlocks(badBlocks, self)
    }

    case Blink(power: Int) if power >= MIN_BLINK_POWER => {
      if (timeSinceBlink <= DOUBLEBLINK) {
        snakeActor ! TurnLeft
        lastBlink = 0
      } else {
        lastBlink = System.currentTimeMillis
      }
    }
    case EEG(_, _, PoorSignalLevel(level)) => {
      if (level > 0) {
        screen.processPoorSignal()
      } else {
        screen.imageToShow = screen.connectedImage
      }
    }
    case d: Direction => {
      snakeActor ! d
    }
    case FruitEaten => {
      host.eatFruit
      fruit = Fruit(GRID_SIZE, GRID_SIZE, badBlocks, screen.boxWidth, screen.boxHeight)
    }

    case StopGame => {
      host.enableChoosers
      context.become(stopped orElse common)
    }
    case PauseGame => {
      paused = true
    }
  }

  def stopped: Receive = {
    case StartGame => {
      badBlocks = newBadBlocks
      host.disableChoosers
      context.become(running orElse common)
    }
    case UnpauseGame => {
      paused = false
    }
    case Reset => {
      snakeActor ! Reset
      badBlocks = newBadBlocks
    }
  }

  def common: Receive = {
    case PoorSignalLevel(level: Int) => {
      screen.processPoorSignal()
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

    case Draw(g, _) => {
      drawBadBlocks(g)
      fruit.draw(g)
      snakeActor ! Draw(g, self)
    }

    case DoneDrawing(_) => {
      screen.screenActor ! DoneDrawing(self)
    }
  }

  def receive = stopped orElse common
}

class NeuroSender extends Thread {
  val host = SnakeyApp
  def screen = SnakeyApp.screen

  var waitingForStop = false

  val snakeyActor = host.system.actorFor("/user/snakeyActor")

  private lazy val in: NeuroIterator = {
    Try(new NeuroIterator) match {
      case Success(v) => {
        host.setInfo("Succesfully connected!")
        host.enableChoosers
        v
      }

      case Failure(e) => {
        host.setInfo("Please check that ThinkGearConnector is on.")
        screen.imageToShow = screen.nosignalImage
        in
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