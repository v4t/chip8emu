package torvi

import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import scalafx.Includes._
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

class ScreenCanvas extends Canvas {

  private val PixelSize = 10
  private val gc = graphicsContext2D

  gc.fill = Color.White

  def clear(): Unit = {
//    gc.clearRect(0, 0, width.value, height.value)
  }

  def setPixels(x: Int, y: Int): Unit = {
    gc.fillRect(x * PixelSize, y * PixelSize, PixelSize, PixelSize)
//    println("x: " + x + " y: " + y + " w: " + width.value + " h " + height.value)
  }

  def initEventHandlers(): Unit = {
    addEventHandler(MouseEvent.MOUSE_CLICKED, clickEventHandler)
  }

  private val clickEventHandler = new EventHandler[MouseEvent] {
    override def handle(e: MouseEvent): Unit = {
      val x = ((e.x - (e.x % PixelSize)) / PixelSize).toLong
      val y = ((e.y - (e.y % PixelSize)) / PixelSize).toLong
      gc.fillRect(x * PixelSize, y * PixelSize, PixelSize, PixelSize)
    }
  }
}