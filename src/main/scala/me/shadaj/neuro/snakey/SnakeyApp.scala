package me.shadaj.neuro.snakey

import java.awt.Color
import java.awt.event._

import javax.swing.Timer

import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

import scala.swing._
import scala.swing.event._

import scala.util.Success
import scala.util.Failure
import scala.util.Try

import scala.annotation.tailrec

import me.shadaj.neuro.thinkgear._

object SnakeyLauncher extends SnakeyApp

class SnakeyApp extends SimpleSwingApplication {
  var game = new SnakeyScreen(SnakeyApp.this, 0, true)
  val panel = new SnakeyPanel(SnakeyApp.this)

  var mindControl = true

  val frameContents = new BoxPanel(Orientation.Vertical) {
    contents += game
    contents += panel

    listenTo(this.keys)
    reactions += {
      case KeyPressed(_, k, _, _) =>
        game.processKey(k)
    }

    focusable = true
  }

  def top = new MainFrame {
    title = "Snakey"
    contents = frameContents

    size = new Dimension(1000, 1000)

    panel.maximumSize = new Dimension(1000, 50)
  }

  var fruitsEaten = 0

  def setInfo(text: String) {
    try panel.setInfo(text) catch {
      case e: Throwable =>
    }
  }

  def die {
    panel.die
  }

  def badSignal {
    if (!game.dead) {
      panel.badSignal
    }
  }

  def eatFruit {
    fruitsEaten += 1
    panel.fruits.text = "Fruits eaten: " + fruitsEaten
  }

  def start {
    if (!game.dead) {
      game.start
      panel.startButton.enabled = false
      panel.levelChooser.enabled = false
    } else {
      game.end
      panel.fruits.text = "Fruits eaten: 0"
      reset
    }
  }

  def reset {
    fruitsEaten = 0
    frameContents.contents.clear
    game = new SnakeyScreen(SnakeyApp.this, panel.levelChooser.selection.item, mindControl)
    frameContents.contents += game
    frameContents.contents += panel
    panel.startButton.enabled = true
    panel.revalidate
  }

  def enableChoosers {
    if (panel != null) {
      panel.enable
    }
  }
}