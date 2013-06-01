package me.shadaj.neuro.snakey

import java.awt.Graphics2D

trait Sprite {
  def draw(g: Graphics2D): Unit
}