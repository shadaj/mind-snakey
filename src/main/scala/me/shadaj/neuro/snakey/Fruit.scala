package me.shadaj.neuro.snakey

class Fruit(val x: Int, val y: Int)

object Fruit {
  def apply(width: Int, height: Int) = {
    new Fruit((Math.random * width).toInt, (Math.random * height).toInt)
  }
}