// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package Le_assignment_01

import widgets.MyCanvas
import utils._

import scala.io.BufferedSource
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.FileChooser

object Main extends JFXApp {

  var vertexes: List[Vertex] = Nil
  var faces   : List[Face]   = Nil

  val canvas = new MyCanvas()

  stage = new PrimaryStage {
    title = "Assignment 01"
    scene = new Scene(800, 600) {
      stylesheets add "css/modena/modena.css"
      root = new BorderPane {
        top = new VBox {
          content = List(toolBar)
        }
        center = canvas
      }
    }

    width onChange((_, _, _)  => { canvas.clear(); canvas.draw(vertexes, faces) })
    height onChange((_, _, _) => { canvas.clear(); canvas.draw(vertexes, faces) })
  }

  lazy val toolBar = new ToolBar {
    val filePathField = new TextField {
      prefWidth = 200
    }

    content = List(
      new Label("Filename:"),
      filePathField,
      new Button("Browse") {
        onAction = (ae: ActionEvent) => filePathField.text = new FileChooser().showOpenDialog(scene.window.get).getAbsolutePath
      },
      new Button("Load") {
        onAction = (ae: ActionEvent) => onLoadBtnClicked(filePathField.getText)
      }
    )
  }
  
  private def onLoadBtnClicked(path: String): Unit = {
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
