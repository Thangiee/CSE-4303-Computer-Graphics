// Le, Thang
// 1000-787-155
// 2015-02-16
// Assignment_02

package Le_assignment_02

import javafx.animation.{KeyFrame, Timeline}
import javafx.event.{ActionEvent, EventHandler}

import utils._
import widgets.{MyCanvas, MyToolBar}

import scala.io.BufferedSource
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.util.Duration

object Main extends JFXApp {

  var vertexes: List[Vertex] = Nil
  var faces   : List[Face]   = Nil

  var vrp: VRP = _
  var vpn: VPN = _
  var vup: VUP = _
  var prp: PRP = _
  var viewVol: ViewVolume = _

  val canvas  = new MyCanvas()
  val toolbar = new MyToolBar()
  toolbar.onLoadButtonClick(handleLoadBtnClick)
  toolbar.onRotateButtonClick(rotateFigure)
  toolbar.onScaleButtonClick(scaleFigure)
  toolbar.onTranslateButtonClick(translateFigure)
  toolbar.onFlyButtonClick(println)

  stage = new PrimaryStage {
    title = "Assignment 03"
    scene = new Scene(800, 600) {
      stylesheets add "css/modena/modena.css"
      root = new BorderPane {
        top = toolbar
        center = canvas
      }
    }

    width onChange((_, _, _)  => { canvas.clear(); canvas.draw(vertexes, faces) })
    height onChange((_, _, _) => { canvas.clear(); canvas.draw(vertexes, faces) })
  }

  private def parseFile(file: BufferedSource): Unit = {
    val lines = file.getLines().mkString("\n").split("\n") // split by lines
    lines.map { line =>
      line.head match {
        case 'v' => vertexes = vertexes :+ parseVertex(line)
        case 'f' => faces = faces :+ parseFace(line)
        case 'r' => vrp = parseVRP(line)
        case 'n' => vpn = parseVPN(line)
        case 'u' => vup = parseVUP(line)
        case 'p' => prp = parsePRP(line)
        case 'w' => viewVol = parseViewVolume(line)
        case 's' => canvas.setViewPort(parseViewport(line))
      }
    }
  }

  private def handleLoadBtnClick(path: String): Unit = {
    val file = io.Source.fromFile(path) // load the file from the path
    vertexes = Nil
    faces = Nil
    parseFile(file)

    // Translate VRP to Origin
    val t = translate(-vrp.x, -vrp.y, -vrp.z)
    vrp = t x vrp

    // get the matrix to rotate around x util VPN lies in the xz plane positively
    val rx = xRotateToPlaneXZ(vpn)
    vrp = rx x vrp
    vpn = rx x vpn
    vup = rx x vup

    // get the matrix to rotate around y util VPN align with the positive x-axis
    val ry = yRotateToAlignZ(vpn)
    vrp = ry x vrp
    vpn = ry x vpn
    vup = ry x vup

    // get the matrix to rotate around z util VUP lies in the yz plane positively
    val rz = zRotateToPlaneYZ(vup)
    vrp = rz x vrp
    vpn = rz x vpn
    vup = rz x vup

    // find the DOP and shear it to become parallel to the z-axis
    val cw = viewVol.getCenterWindow
    val shx = shearXZ((cw.x - prp.x) / prp.z)
    val shy = shearYZ((cw.y - prp.y) / prp.z)
    vrp = Seq(shx, shy).reduce(_ * _) x vrp
    prp = Seq(shx, shy).reduce(_ * _) x prp

    // translate center of window on the front plant (nmin) to the origin
    val t2 = translate(-cw.x, -cw.y, -less(viewVol.maxN, viewVol.minN))
    vrp = t2 x vrp
    prp = t2 x prp

    // scale to canonical Viewing Volume (2 x 2 x 1)
    val sc = scale(
      sx = 2 / math.abs(viewVol.maxU - viewVol.minU),
      sy = 2 / math.abs(viewVol.maxV - viewVol.minV),
      sz = 1 / math.abs(viewVol.maxN - viewVol.minN)
    )
    vrp = sc x vrp
    prp = sc x prp

    val compositeMatrix = Seq(sc, t2, shy, shx, rz, ry, rx, t).reduce(_ * _)
    println(compositeMatrix)
    println(s"$vrp $vpn $vup $prp $viewVol")
    vertexes = vertexes.map(v => compositeMatrix x v)

    canvas.setViewVolume(ViewVolume(0, 1, 0, 1, 0, 0))
    canvas.clear()
    canvas.draw(vertexes, faces)
  }

  private def rotateFigure(rotation: Rotation): Unit = {
    val degree = rotation.degree / rotation.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation

    // rotation matrix
    val m = rotation.axis match {
      case "X" => xRotate(degree)
      case "Y" => yRotate(degree)
      case "Z" => zRotate(degree)
    }

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        vertexes = vertexes.map(v => m x v)
        canvas.clear()
        canvas.draw(vertexes, faces)
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(rotation.steps)
    animation.play()
  }

  private def scaleFigure(scaling: Scaling): Unit = {
    val sx = (scaling.x - 1) / scaling.steps
    val sy = (scaling.y - 1) / scaling.steps
    val sz = (scaling.z - 1) / scaling.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation
    var i = 1

    def toScaleFactor(value: Double) = (1 + value * i) / (1 + value * (i - 1))

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        // scale matrix
        val m = scale(toScaleFactor(sx), toScaleFactor(sy), toScaleFactor(sz))
        vertexes = vertexes.map(v => m x v)
        canvas.clear()
        canvas.draw(vertexes, faces)
        i = i + 1
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(scaling.steps)
    animation.play()
  }

  private def translateFigure(translation: Translation): Unit = {
    val dx = translation.dx / translation.steps
    val dy = translation.dy / translation.steps
    val dz = translation.dz / translation.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation

    val m = translate(dx, dy, dz)
    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        vertexes = vertexes.map(v => m x v)
        canvas.clear()
        canvas.draw(vertexes, faces)
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(translation.steps)
    animation.play()
  }
}
