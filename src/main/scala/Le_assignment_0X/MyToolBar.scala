package Le_assignment_0X

import utils._

import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.control._
import scalafx.scene.layout.{HBox, VBox}
import scalafx.stage.FileChooser

class MyToolBar() extends ToolBar {
  private var loadBtnClickListener     : Option[String => Unit]      = None
  private var rotateBtnClickListener   : Option[Rotation => Unit]    = None
  private var scaleBtnClickListener    : Option[Scaling => Unit]     = None
  private var translateBtnClickListener: Option[Translation => Unit] = None
  private var flyBtnClickListener      : Option[Fly => Unit]         = None

  content = new VBox {
    spacing = 8
    children = List(
      new HBox(spacing = 4) {children = toolbar1; alignment = Pos.Center},
      new HBox(spacing = 4) {children = toolbar2; alignment = Pos.Center},
      new HBox(spacing = 4) {children = toolbar3; alignment = Pos.Center},
      new HBox(spacing = 4) {children = toolbar4; alignment = Pos.Center},
      new HBox(spacing = 4) {children = toolbar5; alignment = Pos.Center}
    )
  }

  private def toolbar1 = {
    val filePathField = new TextField {prefWidth = 200; text = "pyramid_04.txt"}

    List(
      new Label("Filename:"),
      filePathField,
      new Button("Browse") {
        onAction = (ae: ActionEvent) => filePathField.text = new FileChooser().showOpenDialog(scene.window.get).getAbsolutePath
      },
      new Button("Load") {
        onAction = (ae: ActionEvent) => loadBtnClickListener.notify(filePathField.getText)
      }
    )
  }

  private def toolbar2 = {
    val axisToggle  = new ToggleGroup()
    val degreeField = new TextField {prefWidth = 50; text = "90"}
    val stepsField  = new TextField {prefWidth = 50; text = "100"}
    val axisX       = new RadioButton("X") {toggleGroup = axisToggle; selected = true}
    val axisY       = new RadioButton("Y") {toggleGroup = axisToggle}
    val axisZ       = new RadioButton("Z") {toggleGroup = axisToggle}

    def selectedAxis: String = {
      if (axisX.selected.value) "X"
      else if (axisY.selected.value) "Y"
      else "Z"
    }

    List(
      new Label("Roation Axis:"),
      axisX, axisY, axisZ,
      new Label("Degree:"),
      degreeField,
      new Label("Steps:"),
      stepsField,
      new Button("Rotate") {
        onAction = (ae: ActionEvent) => rotateBtnClickListener.notify(Rotation(degreeField.getText.toDouble, stepsField.getText.toInt, selectedAxis))
      }
    )
  }

  private def toolbar3 = {
    val scaleToggle = new ToggleGroup()
    val scaleAll    = new RadioButton("All") {toggleGroup = scaleToggle; selected = true}
    val scaleXYZ    = new RadioButton("[Sx,Sy,Sz] A:") {toggleGroup = scaleToggle}

    val scaleAllAmountField = new TextField {prefWidth = 50; text = "2.0"}
    val scaleXYZAmountField = new TextField {prefWidth = 85; text = "[1,1,1]"; disable = true}
    val stepsField = new TextField {prefWidth = 50; text = "100"}

    scaleAll.onAction = (ae: ActionEvent) => {scaleAllAmountField.disable = false; scaleXYZAmountField.disable = true}
    scaleXYZ.onAction = (ae: ActionEvent) => {scaleAllAmountField.disable = true; scaleXYZAmountField.disable = false}

    // get the factor for which x, y, and z are scaled
    def factor = if (scaleAll.selected.value) {
      val value = scaleAllAmountField.getText.toDouble
      (value, value, value) // x, y, and z are same value
    } else {
      val values = scaleXYZAmountField.getText.replace("[", "").replace("]", "").split(",").map(_.toDouble) // parse the x, y, and z
      (values(0), values(1), values(2))
    }

    List(
      new Label("Scale Ratio:"),
      scaleAll,
      scaleAllAmountField,
      scaleXYZ,
      new TextField {prefWidth = 85; text = "[0.0,0.0,0.0]"; disable = true},
      scaleXYZAmountField,
      new Label("Steps:"),
      stepsField,
      new Button("Scale") {
        onAction = (ae: ActionEvent) =>
          scaleBtnClickListener.notify(Scaling(factor._1, factor._2, factor._3, stepsField.getText.toInt))
      }
    )
  }

  private def toolbar4 = {
    val translationAmountField = new TextField {prefWidth = 85; text = "[1,1,1]"}
    val stepsAmountField       = new TextField {prefWidth = 50; text = "100"}

    List(
      new Label("Translation ([dx, dy, dz]):"),
      translationAmountField,
      new Label("Steps:"),
      stepsAmountField,
      new Button("Translate") {
        onAction = (ae: ActionEvent) => {
          val values = translationAmountField.getText.replace("[", "").replace("]", "").split(",").map(_.toDouble)
          translateBtnClickListener.notify(Translation(values(0), values(1), values(2), stepsAmountField.getText.toInt))
        }
      }
    )
  }

  private def toolbar5 = {
    val stepsAmountField = new TextField {prefWidth = 50; text = "100"}
    val vrp1Field        = new TextField {prefWidth = 85; text = "[0,0.5,0]"}
    val vrp2Field        = new TextField {prefWidth = 85; text = "[0.5,0,0]"}

    List(
      new Label("VRP1([x,y,z]):"),
      vrp1Field,
      new Label("VRP2([x,y,z]):"),
      vrp2Field,
      new Label("Steps"),
      stepsAmountField,
      new Button("Fly") {
        onAction = (ae: ActionEvent) => {
          val values1 = vrp1Field.getText.replace("[", "").replace("]", "").split(",").map(_.toDouble)
          val vrp1 = VRP(values1(0), values1(1), values1(2))
          val values2 = vrp2Field.getText.replace("[", "").replace("]", "").split(",").map(_.toDouble)
          val vrp2 = VRP(values2(0), values2(1), values2(2))
          flyBtnClickListener.notify(Fly(vrp1, vrp2, stepsAmountField.getText.toInt))
          // swap points
          val temp = vrp1Field.getText
          vrp1Field.text = vrp2Field.getText
          vrp2Field.text = temp
        }
      }
    )
  }

  def onLoadButtonClick(listener: String => Unit) = loadBtnClickListener = Some(listener)

  def onRotateButtonClick(listener: Rotation => Unit) = rotateBtnClickListener = Some(listener)

  def onScaleButtonClick(listener: Scaling => Unit) = scaleBtnClickListener = Some(listener)

  def onTranslateButtonClick(listener: Translation => Unit) = translateBtnClickListener = Some(listener)

  def onFlyButtonClick(listener: Fly => Unit) = flyBtnClickListener = Some(listener)

  implicit class Option2Notify[T](option: Option[T => Unit]) {
    def notify(t: T) = option.map(_(t))
  }

}
