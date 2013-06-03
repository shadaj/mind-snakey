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

case class CheckBadBlocks(b: Seq[(Int, Int)], snakeyActor: ActorRef)

case object Reset

class SnakeActor extends Actor {
  val host = SnakeyApp
  def screen = SnakeyApp.screen

  var snake = newSnake
  
  def newSnake = {
    new Snake(Seq(new Part(screen.middleOfGrid, screen.middleOfGrid, Up)), screen.boxWidth, screen.boxHeight)
  }
  
  val snakeyActor = context.actorFor("/user/snakeyActor")

  def receive = {
    case d: Direction => snake = snake.turn(d)

    case CheckEating(f: Fruit, snakeyActor: ActorRef) => {
      if (snake.eating(f)) {
        self ! EatFruit
      }
    }

    case EatFruit => {
      snake = snake.grow
      snakeyActor ! FruitEaten
    }

    case Move => snake = snake.move

    case Draw(g, snakeyActor) => {
      snake.draw(g)
      snakeyActor ! DoneDrawing(self)
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

    case Reset => snake = newSnake
  }
}