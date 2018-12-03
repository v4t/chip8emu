package torvi

import java.nio.file.{Files, Paths}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Scene}
import scalafx.scene.paint.Color._

object Main extends JFXApp {
  def exitWithError(err: String) = {
    println(err)
    sys.exit(0)
  }
  if (parameters.unnamed.length == 0)
    exitWithError("Please provide ROM")
  if (!Files.exists(Paths.get(parameters.unnamed(0))))
    exitWithError("Given ROM file does not exist")

  val rom = parameters.unnamed(0)
  stage = new PrimaryStage {
    title = "C8Emu"
    width = 650
    height = 450
    scene = new Scene {
      fill = Black
    }
  }
}
