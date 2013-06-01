package me.shadaj.neuro.snakey

import scala.collection.mutable
import java.awt.Graphics2D
import java.awt.Color

sealed abstract class Direction(leftDirection: => Direction, rightDirection: => Direction) {
  lazy val left = leftDirection
  lazy val right = rightDirection
  def opposite: Direction = right.right

}

object Down extends Direction(Right, Left)
object Right extends Direction(Up, Down)
object Left extends Direction(Down, Up)
object Up extends Direction(Left, Right)

case class Part(x: Int, y: Int, direction: Direction) {
  def moveInDirection(d: Direction) = {
    d match {
      case Up => Part(x, y - 1, d)
      case Down => Part(x, y + 1, d)
      case Left => Part(x - 1, y, d)
      case Right => Part(x + 1, y, d)
      case _ => Part(x, y, d)
    }
  }
}

class Snake(val parts: Seq[Part], boxWidth: => Int, boxHeight: => Int) extends Sprite {
  def draw(g: Graphics2D) {
    g.setPaint(Color.green)
    parts.foreach {
      case Part(x, y, _) =>
        g.fillRect(x * boxWidth, y * boxHeight, boxWidth, boxHeight)
    }
  }

  def grow: Snake = {
    val partToAdd = parts.last.moveInDirection(parts.last.direction)
    new Snake(parts :+ partToAdd, boxWidth, boxHeight)
  }

  def move: Snake = {
    val newHead = parts.head.moveInDirection(parts.head.direction)
    new Snake(newHead +: parts.init, boxWidth, boxHeight)
  }

  def turn(d: Direction): Snake = {
    new Snake(parts.updated(0, new Part(parts.head.x, parts.head.y, d)), boxWidth, boxHeight)
  }

  def direction = parts.head.direction

  def reverse = new Snake(parts.reverse.map(p => new Part(p.x, p.y, p.direction.opposite)), boxWidth, boxHeight)

  def eatingSelf = parts.tail.exists {
    case Part(x, y, _) =>
      parts.head.x == x && parts.head.y == y
  }
  
  def eating(f: Fruit) = parts.head.x == f.x && parts.head.y == f.y
  
  var dead = false
  
  def die {dead = true}
}

object TestApp extends App {
  println(s"${Down.right}")
  println(s"${Up.right}")
  println(s"${Left.right}")
  println(s"${Right.right}")
}