package me.shadaj.neuro.snakey

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Ellipse2D

class Fruit(val x: Int, val y: Int, width: => Float, height: => Float) extends Sprite {
  def draw(g: Graphics2D) {
    g.setPaint(Color.red)
    g.fill(new Ellipse2D.Float(x * width, y * height, width, height))
  }
}

object Fruit {
  def apply(gridWidth: Int, gridHeight: Int, badBlocks: Seq[(Int, Int)], boxWidth: => Float, boxHeight: => Float): Fruit = {
    val x = (Math.random * gridWidth).toInt
    val y = (Math.random * gridHeight).toInt
    if (badBlocks.contains((x, y))) {
      apply(gridWidth, gridHeight, badBlocks, boxWidth, boxHeight)
    } else {
      new Fruit(x, y, boxWidth, boxHeight)
    }
  }
}