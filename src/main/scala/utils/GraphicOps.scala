package utils

import breeze.linalg._
import math._

trait GraphicOps {

  def translation(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0, dx),
      (0.0, 1.0, 0.0, dy),
      (0.0, 0.0, 1.0, dz),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def scale(sx: Double = 1.0, sy: Double = 1.0, sz: Double = 1.0): DenseMatrix[Double] = {
    DenseMatrix(
      ( sx, 0.0, 0.0, 0.0),
      (0.0,  sy, 0.0, 0.0),
      (0.0, 0.0,  sz, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def xRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg))
    val s = sin(toRadians(deg))
    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0,   c,  -s, 0.0),
      (0.0,   s,   c, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def yRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg))
    val s = sin(toRadians(deg))
    DenseMatrix(
      (  c, 0.0,   s, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      ( -s, 0.0,   c, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def zRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg))
    val s = sin(toRadians(deg))
    DenseMatrix(
      (  c,  -s, 0.0, 0.0),
      (  s,   c, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  implicit class RichDenseMatrix(matrix: DenseMatrix[Double]) {
    def translation(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = matrix * utils.translation(dx, dy, dz)
    def scale(sx: Double = 1.0, sy: Double = 1.0, sz: Double = 1.0): DenseMatrix[Double] = matrix * utils.scale(sx, sy, sz)
    def xRotate(deg: Double): DenseMatrix[Double] = matrix * utils.xRotate(deg)
    def yRotate(deg: Double): DenseMatrix[Double] = matrix * utils.yRotate(deg)
    def zRotate(deg: Double): DenseMatrix[Double] = matrix * utils.zRotate(deg)
  }

  implicit class RichDenseVector(vector: DenseVector[Double]) {
    def toVertex: Vertex = Vertex(vector(0), vector(1), vector(2))
  }

}

object GraphicOps extends GraphicOps
