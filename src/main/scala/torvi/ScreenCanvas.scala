package torvi

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

class ScreenCanvas extends Canvas {
  private val PixelSize = 10
  private val gc = graphicsContext2D

  gc.fill = Color.White

  def clear(): Unit = {
    gc.clearRect(0, 0, width.value, height.value)
  }

  def drawPixels(pixels: Array[Boolean]): Unit = {
    for (i <- pixels.indices) {
      val x = i % 64
      val y = i / 64
      if (pixels(i)) gc.fillRect(x * PixelSize, y * PixelSize, PixelSize, PixelSize)
    }
  }
}