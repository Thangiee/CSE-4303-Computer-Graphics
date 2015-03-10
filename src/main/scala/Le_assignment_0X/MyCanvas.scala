// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package Le_assignment_0X

import utils._

import scalafx.Includes._
import scalafx.scene.Cursor
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color

class MyCanvas extends Pane {
  private implicit var _viewport = Viewport(0, 0, 0, 0)
  private implicit var _viewVol  = ViewVolume(0, 0, 0, 0, 0, 0)
  private implicit var _window   = Window(0, 0, 0, 0)
  
  private val coordinate = new Label()
  private val canvas     = new Canvas()
  private val gc         = canvas.graphicsContext2D
  private var _vpn       = VPN(0, 0, 0)
  private var _vup       = VUP(0, 0, 0)

  style = "-fx-background-color: #FFFF00"
  cursor = Cursor.CROSSHAIR
  content = List(canvas, coordinate)

  width onChange  ((_, _, newW) => canvas.width = newW.doubleValue())
  height onChange ((_, _, newH) => canvas.height = newH.doubleValue())

  def draw(vertexes: Seq[Vertex], faces: Seq[Face], proj: Projection): Unit = {
    val w = canvas.getWidth
    val h = canvas.getHeight

    def drawEdge(v: (Vertex, Vertex)): Unit = {
      // do clipping then draw the edge
      proj match {
        case Projection.Parallel =>
          clipLine(v._1, v._2, viewVolume).map { case clippedVertexes =>
            val (v1, v2) = (clippedVertexes._1.paraMapToViewport, clippedVertexes._2.paraMapToViewport)
            gc.strokeLine(w * v1.x, h * v1.y, w * v2.x, h * v2.y)
          }

        case Projection.Perspective =>
          SutherlandHodgman.clip(v._1.perMapToViewport, v._2.perMapToViewport, viewport, w, h).map {
            case (v1, v2) => gc.strokeLine(v1.x, v1.y, v2.x, v2.y)
          }
      }
    }

    drawViewport()

    faces.map { f =>
      val v1 = vertexes(f.k - 1)
      val v2 = vertexes(f.l - 1)
      val v3 = vertexes(f.m - 1)

      drawEdge(v1, v2)
      drawEdge(v2, v3)
      drawEdge(v3, v1)
    }
  }

  def drawViewport(): Unit = {
    val w = canvas.getWidth
    val h = canvas.getHeight

    gc.strokeRect(
      w * _viewport.minX, h * _viewport.minY,
      w * _viewport.maxX - w * _viewport.minX,
      h * _viewport.maxY - h * _viewport.minY
    )
  }

  def vpn = _vpn
  def vpn_=(vpn: VPN) = _vpn = vpn

  def vup = _vup
  def vup_=(vup: VUP) = _vup = vup

  def viewport = _viewport
  def viewport_=(vp: Viewport) = _viewport = vp

  def viewVolume = _viewVol
  def viewVolume_=(vv: ViewVolume) = _viewVol = vv

  def window = _window
  def window_=(window: Window) = _window = window

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
