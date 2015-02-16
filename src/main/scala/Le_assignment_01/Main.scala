// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package Le_assignment_01

import utils._
import widgets.{MyCanvas, MyToolBar}

import scala.io.BufferedSource
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane

object Main extends JFXApp {

  var vertexes: List[Vertex] = Nil
  var faces   : List[Face]   = Nil

  val canvas = new MyCanvas()
  val toolbar = new MyToolBar()
  toolbar.onLoadButtonClick(handleLoadBtnClick)
  toolbar.onRotateButtonClick(r => println(s"${r.degree}, ${r.steps}, ${r.selectedAxis}"))

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
}
