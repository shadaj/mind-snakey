package me.shadaj.neuro.snakey

import scala.collection.mutable

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
      case Up => Part(x,y-1, d)
      case Down => Part(x,y+1, d)
      case Left => Part(x-1,y,d)
      case Right => Part(x+1,y, d)
      case _ => Part(x,y,d)
    }
  }
}

class Snake(val parts: Seq[Part]) {
  def grow: Snake = {
    val partToAdd = parts.last.moveInDirection(parts.last.direction)
    new Snake(parts :+ partToAdd)
  }
  
  def move: Snake = {
    val newHead = parts.head.moveInDirection(parts.head.direction)
    new Snake(newHead +: parts.init)
  }
  
  def turn(d: Direction): Snake = {
    new Snake(parts.updated(0, new Part(parts.head.x, parts.head.y, d)))
  }
  
  def direction = parts.head.direction
}

object TestApp extends App {
  println(s"${Down.right}")
  println(s"${Up.right}")
  println(s"${Left.right}")
  println(s"${Right.right}")
}