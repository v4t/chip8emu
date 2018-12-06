package torvi

import java.nio.file.{Files, Paths}

import scalafx.scene.input.KeyCode
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
  stage = new PrimaryStage {
    title = "CH8Emu"
    resizable = false
    scene = new Scene {
      fill = Black
      onKeyPressed = k => k.code match {
        case KeyCode.Escape => sys.exit(0)
        case KeyCode.Digit1 => emulator.keyboardInput(0x1) = true
        case KeyCode.Digit2 => emulator.keyboardInput(0x2) = true
        case KeyCode.Digit3 => emulator.keyboardInput(0x3) = true
        case KeyCode.Digit4 => emulator.keyboardInput(0xc) = true
        case KeyCode.Q => emulator.keyboardInput(0x4) = true
        case KeyCode.W => emulator.keyboardInput(0x5) = true
        case KeyCode.E => emulator.keyboardInput(0x6) = true
        case KeyCode.R => emulator.keyboardInput(0xd) = true
        case KeyCode.A => emulator.keyboardInput(0x7) = true
        case KeyCode.S => emulator.keyboardInput(0x8) = true
        case KeyCode.D => emulator.keyboardInput(0x9) = true
        case KeyCode.Z => emulator.keyboardInput(0xe) = true
        case KeyCode.X => emulator.keyboardInput(0xa) = true
        case KeyCode.C => emulator.keyboardInput(0x0) = true
        case KeyCode.V => emulator.keyboardInput(0xf) = true
        case _ => Nil
      }
      onKeyReleased = k => k.code match {
        case KeyCode.Digit1 => emulator.keyboardInput(0x1) = false
        case KeyCode.Digit2 => emulator.keyboardInput(0x2) = false
        case KeyCode.Digit3 => emulator.keyboardInput(0x3) = false
        case KeyCode.Digit4 => emulator.keyboardInput(0xc) = false
        case KeyCode.Q => emulator.keyboardInput(0x4) = false
        case KeyCode.W => emulator.keyboardInput(0x5) = false
        case KeyCode.E => emulator.keyboardInput(0x6) = false
        case KeyCode.R => emulator.keyboardInput(0xd) = false
        case KeyCode.A => emulator.keyboardInput(0x7) = false
        case KeyCode.S => emulator.keyboardInput(0x8) = false
        case KeyCode.D => emulator.keyboardInput(0x9) = false
        case KeyCode.Z => emulator.keyboardInput(0xe) = false
        case KeyCode.X => emulator.keyboardInput(0xa) = false
        case KeyCode.C => emulator.keyboardInput(0x0) = false
        case KeyCode.V => emulator.keyboardInput(0xf) = false
        case _ => Nil
      }
      root = new BorderPane {
        val timeline = new Timeline {
          cycleCount = Timeline.Indefinite
//          keyFrames = KeyFrame(Duration(3.1), onFinished = (e: ActionEvent) => {
          keyFrames = KeyFrame(Duration(400), onFinished = (e: ActionEvent) => {
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
