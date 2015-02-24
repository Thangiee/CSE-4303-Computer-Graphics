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
  implicit var viewVolume = ViewVolume(0, 0, 0, 0, 0, 0)
  private  val coordinate = new Label()
  private  val canvas     = new Canvas()
  private  val gc         = canvas.graphicsContext2D


  style = "-fx-background-color: #FFFF00"
  cursor = Cursor.CROSSHAIR
  content = List(canvas, coordinate)

  width onChange  ((_, _, newW) => canvas.width = newW.doubleValue())
  height onChange ((_, _, newH) => canvas.height = newH.doubleValue())

  def draw(vertexes: List[Vertex], faces: List[Face]): Unit = {
    val w = canvas.getWidth
    val h = canvas.getHeight

    def drawEdge(v: (Vertex, Vertex)): Unit = {
      val (v1, v2) = (v._1.mapToViewport(w, h), v._2.mapToViewport(w, h))
      gc.strokeLine(v1.x, v1.y, v2.x, v2.y)
    }

    drawViewport()

    faces.map { f =>
      val v1 = vertexes(f.k - 1)
      val v2 = vertexes(f.l - 1)
      val v3 = vertexes(f.m - 1)

      // do clipping then draw the edge
      clipLine(v1, v2, viewVolume).map(drawEdge)
      clipLine(v2, v3, viewVolume).map(drawEdge)
      clipLine(v3, v1, viewVolume).map(drawEdge)
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

  def setViewVolume(viewVol: ViewVolume): Unit = viewVolume = viewVol

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
