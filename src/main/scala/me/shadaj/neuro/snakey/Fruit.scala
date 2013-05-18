package me.shadaj.neuro.snakey

class Fruit(val x: Int, val y: Int)

object Fruit {
  def apply(width: Int, height: Int, badBlocks: Seq[(Int, Int)]): Fruit = {
    val x = (Math.random * width).toInt
    val y = (Math.random * height).toInt
    if (badBlocks.contains((x,y))) {
      apply(width, height, badBlocks)
    } else {
      new Fruit(x,y)
    }
  }
}