package me.shadaj.neuro.snakey

import scala.swing.GridPanel
import scala.swing.ComboBox
import scala.swing.Button
import scala.swing.Label
import scala.swing.event.ButtonClicked

class SnakeyPanel extends GridPanel(1, 3) {
  val host = SnakeyApp

  val mainInfo = new Label("You're doing great!")
  contents += mainInfo

  def setInfo(text: String) { mainInfo.text = text }

  def disable {
    startButton.enabled = false
    levelChooser.enabled = false
  }

  def enable {
    startButton.enabled = true
    levelChooser.enabled = true
  }

  def die {
    mainInfo.text = "You died ;("
    startButton.enabled = true
    levelChooser.enabled = true
    startButton.text = "Reset"
  }

  def badSignal {
    mainInfo.text = "Looks like your headset isn't on!"
  }

  border = new javax.swing.border.EtchedBorder

  val fruits = new Label("Fruits eaten: None")
  contents += fruits

  this.focusable = false

  val startButton = new Button("Start")
  startButton.focusable = false
  contents += startButton

  val levelChooser = new ComboBox((0 to 20))
  levelChooser.focusable = false
  contents += levelChooser

  listenTo(startButton)

  startButton.reactions += {
    case ButtonClicked(b) =>
      host.start
  }
}