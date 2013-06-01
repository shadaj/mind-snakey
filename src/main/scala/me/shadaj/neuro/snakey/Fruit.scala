package me.shadaj.neuro.snakey

import java.awt.Graphics2D
import java.awt.Color

class Fruit(val x: Int, val y: Int, width: => Int, height: => Int) extends Sprite {
  def draw(g: Graphics2D) {
    g.setPaint(Color.red)
    g.fillOval(x * width, y * height, width, height)
  }
}

object Fruit {
  def apply(width: Int, height: Int, badBlocks: Seq[(Int, Int)], boxWidth: => Int, boxHeight: => Int): Fruit = {
    val x = (Math.random * width).toInt
    val y = (Math.random * height).toInt
    if (badBlocks.contains((x, y))) {
      apply(width, height, badBlocks, boxWidth, boxHeight)
    } else {
      new Fruit(x, y, boxWidth, boxHeight)
    }
  }
}