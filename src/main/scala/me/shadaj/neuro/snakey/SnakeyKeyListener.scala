package me.shadaj.neuro.snakey

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.event.KeyAdapter

class SnakeyKeyListener(game: SnakeyScreen) extends KeyAdapter {
  override def keyPressed(e: KeyEvent) {
    game.snake = game.snake.turn(e.getKeyChar match {
      case 'w' => Up
      case 'a' => Left
      case 's' => Down
      case 'd' => Right
      case _ => game.snake.direction
    })
  }
}