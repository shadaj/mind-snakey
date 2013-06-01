package me.shadaj.neuro.snakey

import java.awt.Graphics2D

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import javax.swing.SwingUtilities

case object Die
case object Move
case object EatFruit
case object BadSignal
case object Start

case object TurnLeft
case object TurnRight

case class Draw(g: Graphics2D, screenActor: ActorRef)

case class CheckEating(f: Fruit, snakeyActor: ActorRef)

case object Eating
case object NotEating

case class CheckBadBlocks(b: Seq[(Int, Int)], snakeyActor: ActorRef)

case object Reset

class SnakeActor extends Actor {
  var snake = new Snake(Seq(new Part(SnakeyApp.screen.middleOfGrid, SnakeyApp.screen.middleOfGrid, Up)), SnakeyApp.screen.boxWidth, SnakeyApp.screen.boxHeight)

  val host = SnakeyApp
  def screen = SnakeyApp.screen

  val snakeyActor = context.actorFor("/user/snakeyActor")
  
  def receive = {
    case d: Direction => snake = snake.turn(d)

    case EatFruit => {
      snakeyActor ! EatFruit
      snake = snake.grow
      host.eatFruit
    }

    case Move => snake = snake.move

    case Draw(g, sa) => {
      SwingUtilities.invokeAndWait(new Runnable {
        def run {
          snake.draw(g)
        }
      })
      
      sa ! DoneDrawing(self)
    }

    case BadSignal => {
      if (!snake.dead) {
        host.panel.badSignal
      }
    }

    case Start => {
      if (!snake.dead) {
        snakeyActor ! StartGame
      } else {
        host.reset
      }

    }

    case Tick => {
      if (snake.eatingSelf) {
        host.die
        snake.die
        snakeyActor ! StopGame
      }
    }

    case CheckEating(f: Fruit, snakeyActor: ActorRef) => {
      if (snake.eating(f)) {
        snake = snake.grow
        host.eatFruit
        snakeyActor ! Eating
      }
    }

    case TurnRight => {
      snake = snake.turn(snake.direction.right)
    }

    case TurnLeft => {
      snake = snake.turn(snake.direction.left)
    }
    
    case CheckBadBlocks(b: Seq[(Int, Int)], snakeyActor: ActorRef) => {
      val headCoordinates = (snake.parts.head.x, snake.parts.head.y)
      if (b.contains(headCoordinates)) {
        snake = snake.reverse.move
      } 
    }
    
    case Reset => snake =  new Snake(Seq(new Part(SnakeyApp.screen.middleOfGrid, SnakeyApp.screen.middleOfGrid, Up)), SnakeyApp.screen.boxWidth, SnakeyApp.screen.boxHeight)
  }
}