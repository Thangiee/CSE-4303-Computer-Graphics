// Le, Thang
// 1000-787-155
// 2015-02-16
// Assignment_02

package Le_assignment_0X

import javafx.animation.{KeyFrame, Timeline}
import javafx.event.{ActionEvent, EventHandler}

import Le_assignment_0X.Projection._
import breeze.linalg.DenseMatrix
import utils._

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.util.Duration

object Main extends JFXApp {

  var path: String = _
  val vertexes = new ListBuffer[Vertex]()
  val faces    = new ListBuffer[Face]()
  val cameras  = new ListBuffer[Camera]()

  var projMatrix: DenseMatrix[Double] = _

  val canvas  = new MyCanvas()
  val toolbar = new MyToolBar()
  toolbar.onLoadButtonClick(handleLoadBtnClick)
  toolbar.onRotateButtonClick(rotateFigure)
  toolbar.onScaleButtonClick(scaleFigure)
  toolbar.onTranslateButtonClick(translateFigure)
  toolbar.onFlyButtonClick(flyCamera)

  stage = new PrimaryStage {
    title = "Assignment 04"
    scene = new Scene(800, 600) {
      stylesheets add "css/modena/modena.css"
      root = new BorderPane {
        top = toolbar
        center = canvas
      }
    }

    width onChange((_, _, _)  => redraw())
    height onChange((_, _, _) => redraw())
  }

  private def redraw(): Unit = {
    canvas.clear()

    // redraw for each of the cameras
    cameras.map { cam =>
      cam.projType match {
        case Projection.Parallel    =>
          canvas.viewVolume = ViewVolume(minU = 0, maxU = 1, minV = 0, maxV = 1, minN = 0, maxN = 1)
          canvas.window = Window(minX = 0, minY = 0, maxX = 1, maxY = 1)
        case Projection.Perspective =>
          canvas.viewVolume = ViewVolume(minU = -1, maxU = 1, minV = -1, maxV = 1, 0, 1)
          canvas.window = Window(minX = -1, minY = -1, maxX = 1, maxY = 1)
      }
      canvas.viewport = cam.viewport
      canvas.draw(vertexes.map(v => cam.modelMatrix x projMatrix x v), faces, cam.projType)
    }
  }

  private def parseFile(file: BufferedSource): Unit = {
    file.getLines().mkString("\n").split("\n").map { line =>
      line.head match {
        case 'v' => vertexes += parseVertex(line)
        case 'f' => faces += parseFace(line)
      }
    }

    io.Source.fromFile("cameras_04.txt").getLines().mkString("\n").split("\n").map { line =>
      line.head match {
        case 'c' => cameras += Camera()
        case 'i' => cameras.last.name = line.split(" ").last
        case 't' => cameras.last.projType = if (line.contains("parallel")) Parallel else Perspective
        case 'r' => cameras.last.vrp = parseVRP(line)
        case 'n' => cameras.last.vpn = parseVPN(line)
        case 'u' => cameras.last.vup = parseVUP(line)
        case 'p' => cameras.last.prp = parsePRP(line)
        case 'w' => cameras.last.viewVolume = parseViewVolume(line)
        case 's' => cameras.last.viewport = parseViewport(line)
      }
    }
  }

  private def handleLoadBtnClick(path: String): Unit = {
    this.path = path
    val file = io.Source.fromFile(path) // load the file from the path
    
    // clear old data if any
    vertexes.clear()
    faces.clear()
    cameras.clear()
    
    parseFile(file)
    cameras.map(_.calcModelMatrix())
    projMatrix = identityMatrix

    cameras.map(println)
    redraw()
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
        redraw()
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
        redraw()
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
        redraw()
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(translation.steps)
    animation.play()
  }

  def flyCamera(fly: Fly): Unit = {
    val vrp1 = fly.vrp1
    val vrp2 = fly.vrp2

    // move camera to vrp 1
    var temp = vrp1
    cameras.map(_.vrp = vrp1)
    cameras.map(_.calcModelMatrix())
    redraw()

    // create matrix to fly to vrp 2
    val dx = (vrp2.x - vrp1.x) / fly.steps
    val dy = (vrp2.y - vrp1.y) / fly.steps
    val dz = (vrp2.z - vrp1.z) / fly.steps
    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        cameras.map(_.vrp = VRP(temp.x + dx, temp.y + dy, temp.z + dz))
        temp = VRP(temp.x + dx, temp.y + dy, temp.z + dz)
        cameras.map(_.calcModelMatrix())
        redraw()
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(fly.steps)
    animation.play()
  }
}
