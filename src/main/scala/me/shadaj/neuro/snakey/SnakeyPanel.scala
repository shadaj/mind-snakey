package me.shadaj.neuro.snakey

import scala.swing.GridPanel
import scala.swing.ComboBox
import scala.swing.Button
import scala.swing.Label
import scala.swing.event.ButtonClicked
import scala.swing.CheckBox
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ToggleButton

class SnakeyPanel extends GridPanel(1, 5) {
  val host = SnakeyApp

  val mainInfo = new Label("You're doing great!")

  def setInfo(text: String) { mainInfo.text = text }

  def disable {
    startButton.enabled = false
    levelChooser.enabled = false
    mindControl.enabled = false
  }

  def enable {
    startButton.enabled = true
    levelChooser.enabled = true
    mindControl.enabled = true
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

  this.focusable = false

  val startButton = new Button("Start")
  startButton.focusable = false

  val levelChooser = new ComboBox((0 to 20))
  levelChooser.focusable = false

  val mindControl = new ToggleButton
  mindControl.text = "Mind Control"
  mindControl.focusable = false
  mindControl.selected = false

  contents += mainInfo
  contents += fruits
  contents += levelChooser
  contents += mindControl
  contents += startButton

  listenTo(startButton)

  startButton.reactions += {
    case ButtonClicked(b) =>
      host.start
  }

  listenTo(mindControl)

  mindControl.reactions += {
    case ButtonClicked(b) => {
      if (mindControl.selected) {
        host.screen.turnOnMindControl
      } else {
        host.screen.turnOffMindControl
        enable
      }
    }
  }
}