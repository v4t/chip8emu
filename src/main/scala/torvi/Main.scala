package torvi

import java.nio.file.{Files, Paths}

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
import torvi.chip8.Emulator

object Main extends JFXApp {
  val args = parameters.unnamed

  if(args.length != 1) {
    println("Usage: /path/to/rom.ch8")
    sys.exit()
  }
  if(!Files.exists(Paths.get(args(0)))) {
    println("Invalid file specified")
    sys.exit()
  }
  val rom = Files.readAllBytes(Paths.get(args(0)))

  println(Paths.get(args(0)))

  val emulator = new Emulator()
  emulator.loadRom(rom)

  val screenCanvas = new ScreenCanvas
  screenCanvas.initEventHandlers()
  stage = new PrimaryStage {

    title = "CH8Emu"
    resizable = false

    scene = new Scene {
      fill = Black

      root = new BorderPane {
        val timeline = new Timeline {
          cycleCount = Timeline.Indefinite
          keyFrames = KeyFrame(Duration(3.1), onFinished = (e: ActionEvent) => {
            emulator.executeCycle()
            screenCanvas.clear()
            screenCanvas.drawScreen(emulator.screenPixels)
//            emulator.debugScreen()
//            println("--------------------------------------------------------------------------------------------------------------------------------------")
          })
        }
        timeline.play()

        center = new Group {
          screenCanvas.width <== 640
          screenCanvas.height <== 320
          children = screenCanvas
        }
      }
    }
  }
}
