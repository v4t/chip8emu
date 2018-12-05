package torvi

import scalafx.Includes._
import scalafx.animation.KeyFrame
import scalafx.animation.Timeline
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.paint.Color._
import scalafx.scene.{Group, Scene}
import scalafx.util.Duration

object Main extends JFXApp {

  val rnd = new scala.util.Random()
  val s1 = 0
  val e1 = 63
  val s2 = 0
  val e2 = 31

  val cellCanvas = new ScreenCanvas
  cellCanvas.initEventHandlers()
  stage = new PrimaryStage {

    title = "CH8Emu"
    resizable = false

    scene = new Scene {
      fill = Black

      root = new BorderPane {
        val timeline = new Timeline {
          cycleCount = Timeline.Indefinite
          keyFrames = KeyFrame(Duration(60), onFinished = (e: ActionEvent) => {
            cellCanvas.clear()
            cellCanvas.setPixels(s1 + rnd.nextInt((e1 - s1) + 1), s2 + rnd.nextInt((e2 - s2) + 1))
          })
        }
        timeline.play()

        center = new Group {
          cellCanvas.width <== 640
          cellCanvas.height <== 320
          children = cellCanvas
        }
      }
    }
  }
}