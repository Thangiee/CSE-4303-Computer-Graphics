// Le, Thang
// 1000-787-155
// 2015-02-16
// Assignment_02

package Le_assignment_0X

import javafx.animation.{KeyFrame, Timeline}
import javafx.event.{ActionEvent, EventHandler}

import breeze.linalg.DenseMatrix
import utils._
import widgets.{MyCanvas, MyToolBar}

import scala.io.BufferedSource
import scala.math.abs
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.util.Duration

object Main extends JFXApp {

  var path: String = _
  var vertexes: List[Vertex] = Nil
  var faces   : List[Face]   = Nil

  var vrp: VRP = _
  var vpn: VPN = _
  var vup: VUP = _
  var prp: PRP = _
  var viewVol: ViewVolume = _

  var modelMatrix: DenseMatrix[Double] = _
  var projMatrix: DenseMatrix[Double] = _

  val canvas  = new MyCanvas()
  val toolbar = new MyToolBar()
  toolbar.onLoadButtonClick(handleLoadBtnClick)
  toolbar.onRotateButtonClick(rotateFigure)
  toolbar.onScaleButtonClick(scaleFigure)
  toolbar.onTranslateButtonClick(translateFigure)
  toolbar.onFlyButtonClick(flyCamera)

  stage = new PrimaryStage {
    title = "Assignment 03"
    scene = new Scene(800, 600) {
      stylesheets add "css/modena/modena.css"
      root = new BorderPane {
        top = toolbar
        center = canvas
      }
    }

    width onChange((_, _, _)  => { canvas.clear(); canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces) })
    height onChange((_, _, _) => { canvas.clear(); canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces) })
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
        case 's' => canvas.viewport = parseViewport(line)
      }
    }
  }

  private def handleLoadBtnClick(path: String): Unit = {
    this.path = path
    val file = io.Source.fromFile(path) // load the file from the path
    vertexes = Nil
    faces = Nil
    parseFile(file)

    modelMatrix = calcParaCompositeMatrix()
    projMatrix = DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )

    println(s"$modelMatrix\n$vrp $vpn $vup $prp $viewVol")

    canvas.vpn = vpn
    canvas.vup = vup
    canvas.viewVolume = ViewVolume(minU = 0, maxU = 1, minV = 0, maxV = 1, minN = 0, maxN = 1)
    canvas.window = Window(minX = 0, minY = 0, maxX = 1, maxY = 1)
    canvas.clear()
    canvas.draw(vertexes.map(v => modelMatrix x v), faces)
  }

  private def rotateFigure(rotation: Rotation): Unit = {
    val degree = rotation.degree / rotation.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation

    // rotation matrix
    val r = rotation.axis match {
      case "X" => xRotate(degree)
      case "Y" => yRotate(degree)
      case "Z" => zRotate(degree)
    }

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        projMatrix = r x projMatrix
        canvas.clear()
        canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces)
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
        val s = scale(toScaleFactor(sx), toScaleFactor(sy), toScaleFactor(sz)) x translate(0, 0, 0)

        projMatrix = projMatrix x s
        canvas.clear()
        canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces)
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

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        projMatrix = translate(dx, dy, dz) x projMatrix
        canvas.clear()
        canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces)
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(translation.steps)
    animation.play()
  }

  def flyCamera(fly: Fly): Unit = {
    // todo: find a better way to do this. eww
    val vrp1 = fly.vrp1
    val vrp2 = fly.vrp2

    // move camera to vrp 1
    vertexes = Nil
    faces = Nil
    val file = io.Source.fromFile(path) 
    parseFile(file)
    vrp = vrp1
    var temp = vrp1
    modelMatrix = calcParaCompositeMatrix()
    canvas.clear()
    canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces)

    // create matrix to fly to vrp 2
    val dx = (vrp2.x - vrp1.x) / fly.steps
    val dy = (vrp2.y - vrp1.y) / fly.steps
    val dz = (vrp2.z - vrp1.z) / fly.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation
    
    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        vertexes = Nil
        faces = Nil
        val file = io.Source.fromFile(path) 
        parseFile(file)
        vrp = VRP(temp.x + dx, temp.y + dy, temp.z + dz)
        temp = vrp
        modelMatrix = calcParaCompositeMatrix()
        canvas.clear()
        canvas.draw(vertexes.map(v => modelMatrix x projMatrix x v), faces)
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(fly.steps)
    animation.play()
  }

  private def calcParaCompositeMatrix(): DenseMatrix[Double] = {
    // Translate VRP to Origin
    val t = translate(-vrp.x, -vrp.y, -vrp.z)
    vrp = t x vrp

    // get the matrix to rotate around x util VPN lies in the xz plane positively
    val rx = xRotateToPlaneXZ(vpn)
    vpn = rx x vpn
    vup = rx x vup

    // get the matrix to rotate around y util VPN align with the positive x-axis
    val ry = yRotateToAlignZ(vpn)
    vpn = ry x vpn
    vup = ry x vup

    // get the matrix to rotate around z util VUP lies in the yz plane positively
    val rz = zRotateToPlaneYZ(vup)
    vpn = rz x vpn
    vup = rz x vup

    // find the DOP and shear it to become parallel to the z-axis
    val cw = viewVol.getCenterWindow
    val shx = shearXZ((cw.x - prp.x) / prp.z)
    val shy = shearYZ((cw.y - prp.y) / prp.z)
    vrp = Seq(shx, shy).reduce(_ * _) x vrp
    prp = Seq(shx, shy).reduce(_ * _) x prp

    // translate center of window on the front plant to the origin
    val t2 = translate(-less(viewVol.maxU, viewVol.minU), -less(viewVol.maxV, viewVol.minV), -less(viewVol.maxN, viewVol.minN))
    vrp = t2 x vrp
    prp = t2 x prp

    // scale to canonical Viewing Volume (1 x 1 x 1)
    val sc = scale(
      sx = 1 / abs(viewVol.maxU - viewVol.minU),
      sy = 1 / abs(viewVol.maxV - viewVol.minV),
      sz = 1 / abs(viewVol.maxN - viewVol.minN)
    )
    vrp = sc x vrp
    prp = sc x prp

    Seq(sc, t2, shy, shx, rz, ry, rx, t).reduce(_ * _)
  }

  private def calcPerCompositeMatrix(): DenseMatrix[Double] = {
    // Translate VRP to Origin
    val t = translate(-vrp.x, -vrp.y, -vrp.z)
    vrp = t x vrp

    // get the matrix to rotate around x util VPN lies in the xz plane positively
    val rx = xRotateToPlaneXZ(vpn)
    vpn = rx x vpn
    vup = rx x vup

    // get the matrix to rotate around y util VPN align with the positive x-axis
    val ry = yRotateToAlignZ(vpn)
    vpn = ry x vpn
    vup = ry x vup

    // get the matrix to rotate around z util VUP lies in the yz plane positively
    val rz = zRotateToPlaneYZ(vup)
    vup = rz x vup

    // find the matrix to translate prp to the origin, but don't do the translation
    // yet since it's easier to find the DOP before the translation
    val t2 = translate(-prp.x, -prp.y, -prp.z)

    val cw = viewVol.getCenterWindow
    val (dopX, dopY) = (cw.x - prp.x, cw.y - prp.y)

    // find the matrix to shear the view volume such that the center line becomes the z-axis
    val shx = shearXZ(dopX / prp.z)
    val shy = shearYZ(dopY / prp.z)

    prp = Seq(t2, shy, shx).reduce(_ * _) x prp
    vrp = Seq(t2, shy, shx).reduce(_ * _) x vrp

    val sc = scale(
      sx = (2 * abs(vrp.z)) / ((viewVol.maxU - viewVol.minU) * (vrp.z + viewVol.maxN)),
      sy = (2 * abs(vrp.z)) / ((viewVol.maxV - viewVol.minV) * (vrp.z + viewVol.maxN)),
      sz = 1 / (vrp.z + viewVol.maxN)
    )

    prp = sc x prp
    vrp = sc x vrp

    Seq(sc, shy, shx, t2, rz, ry, rz, t).reduce(_ * _)
  }
}
