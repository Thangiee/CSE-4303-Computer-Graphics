// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package Le_assignment_01

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

  val canvas = new MyCanvas()
  val toolbar = new MyToolBar()
  toolbar.onLoadButtonClick(handleLoadBtnClick)
  toolbar.onRotateButtonClick(handleRotateBtnClick)

  stage = new PrimaryStage {
    title = "Assignment 01"
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
  
  private def handleLoadBtnClick(path: String): Unit = {
    val file = io.Source.fromFile(path) // load the file from the path
    vertexes = Nil
    faces = Nil
    parseFile(file)
    canvas.clear()
    canvas.draw(vertexes, faces)
  }

  private def parseFile(file: BufferedSource): Unit = {
    val lines = file.getLines().mkString("\n").split("\n") // split by lines
    lines.map { line =>
      line.head match {
        case 'v' => vertexes = vertexes :+ parseVertex(line)
        case 'f' => faces = faces :+ parseFace(line)
        case 'w' => canvas.setWindow(parseWindow(line))
        case 's' => canvas.setViewPort(parseViewport(line))
      }
    }
  }

  private def handleRotateBtnClick(rotation: DoRotation): Unit = {
    val degree = rotation.degree / rotation.steps

    val m = rotation.axis match {
      case "X" => xRotate(degree)
      case "Y" => yRotate(degree)
      case "Z" => zRotate(degree)
    }

    val millis = 2.70078 * math.exp(.00143645 * vertexes.size) // determine duration of each frame for smoother animation

    val frame = new KeyFrame(Duration(millis), new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        vertexes = vertexes.map(_.transform(m))
        canvas.clear()
        canvas.draw(vertexes, faces)
      }
    })

    val animation = new Timeline(frame)
    animation.setCycleCount(rotation.steps)
    animation.play()
  }
}
