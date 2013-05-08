package me.shadaj.neuro.snakey

import scala.collection.mutable

sealed abstract class Direction(left: Direction, right: Direction) {
  def turnLeft: Direction = left
  def turnRight: Direction = right
  def opposite: Direction = right.turnRight
}
object Up extends Direction(Left, Right)
object Down extends Direction(Right, Left)
object Left extends Direction(Down, Up)
object Right extends Direction(Up, Down)

case class Part(x: Int, y: Int, direction: Direction) {
  def moveInDirection(d: Direction) = {
    d match {
      case Up => Part(x,y-1, d)
      case Down => Part(x,y+1, d)
      case Left => Part(x-1,y,d)
      case Right => Part(x+1,y, d)
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