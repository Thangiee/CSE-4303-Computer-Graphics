// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package widgets

import utils._

import scalafx.Includes._
import scalafx.scene.Cursor
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

class MyCanvas extends Pane {
  implicit var viewport   = Viewport(0, 0, 0, 0)
  implicit var window     = Window(0, 0, 0, 0)
  private  val coordinate = new Label()
  private  val canvas     = new Canvas()
  private  val gc         = canvas.graphicsContext2D


  style = "-fx-background-color: #FFFF00"
  cursor = Cursor.CROSSHAIR
  content = List(canvas, coordinate)

  width onChange  ((_, _, newW) => canvas.width = newW.doubleValue())
  height onChange ((_, _, newH) => canvas.height = newH.doubleValue())

  def draw(vertexes: List[Vertex], faces: List[Face], enableColor: Boolean = false): Unit = {
    val w = canvas.getWidth
    val h = canvas.getHeight

    drawViewport()

    faces.map { f =>
      val v1 = vertexes(f.k - 1).mapToViewport
      val v2 = vertexes(f.l - 1).mapToViewport
      val v3 = vertexes(f.m - 1).mapToViewport

      // connect the vertexes
      gc.strokeLine(w * v1.x, h * v1.y, w * v2.x, h * v2.y)
      gc.strokeLine(w * v2.x, h * v2.y, w * v3.x, h * v3.y)
      gc.strokeLine(w * v3.x, h * v3.y, w * v1.x, h * v1.y)

      if (enableColor) {
        // color in the face
        gc.setFill(Color.Red)
        gc.fillPolygon(Seq(
          (w * v1.x) → (h * v1.y),
          (w * v2.x) → (h * v2.y),
          (w * v3.x) → (h * v3.y)
        ))
      }
    }
  }

  def drawViewport(): Unit = {
    val w = canvas.getWidth
    val h = canvas.getHeight
    gc.strokeRect(
      w * viewport.minX, h * viewport.minY,
      w * viewport.maxX - w * viewport.minX,
      h * viewport.maxY - h * viewport.minY
    )
  }

  def setViewPort(vp: Viewport): Unit = viewport = vp

  def setWindow(win: Window): Unit = window = win

  def clear(): Unit = {
    gc.setFill(Color.web("#FFFF00"))
    canvas.graphicsContext2D.fillRect(0, 0, canvas.getWidth, canvas.getWidth)
  }

  filterEvent(MouseEvent.Any) {
    event: MouseEvent =>
      coordinate.text = s"${event.x.toInt}, ${event.y.toInt}"
      coordinate.layoutX = event.x + 10
      coordinate.layoutY = event.y - (if (event.y > 15) 15 else -10)
  }
}
