package utils

import breeze.linalg._
import math._

trait GraphicOps {

  def translate(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = {
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

  def shearXZ(x: Double): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0,   x, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def shearYZ(y: Double): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0,   y, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def xRotateToPlaneXZ(vpn: VPN): DenseMatrix[Double] = {
    val (x, y, z) = (vpn.x, vpn.y, vpn.z)
    val v = sqrt(y*y + z*z)
    val a = if (y*y + z*z == 0) 1 else y / v
    val b = if (y*y + z*z == 0) 1 else z / v

    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0,   b,  -a, 0.0),
      (0.0,   a,   b, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def yRotateToAlignZ(vpn: VPN): DenseMatrix[Double] = {
    val (a, b, c) = (vpn.x, vpn.y, vpn.z)
    val d = sqrt(a*a + c*c)

    DenseMatrix(
      (c/d, 0.0,-a/d, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (a/d, 0.0, c/d, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def zRotateToPlaneYZ(vup: VUP): DenseMatrix[Double] = {
    val (a, b, c) = (vup.x, vup.y, vup.z)
    val d = sqrt(a*a + b*b)

    DenseMatrix(
      (b/d,-a/d, 0.0, 0.0),
      (a/d, b/d, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  implicit class RichDenseMatrix(matrix: DenseMatrix[Double]) {
    def translation(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = matrix * GraphicOps.translate(dx, dy, dz)
    def scale(sx: Double = 1.0, sy: Double = 1.0, sz: Double = 1.0): DenseMatrix[Double] = matrix * GraphicOps.scale(sx, sy, sz)
    def xRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.xRotate(deg)
    def yRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.yRotate(deg)
    def zRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.zRotate(deg)
  }

  implicit class RichDenseVector(vector: DenseVector[Double]) {
    def toVertex: Vertex = Vertex(vector(0), vector(1), vector(2))
    def toVPN = VPN(vector(0), vector(1), vector(2))
    def toVUP = VUP(vector(0), vector(1), vector(2))
    def toPRP = PRP(vector(0), vector(1), vector(2))
    def toVRP = VRP(vector(0), vector(1), vector(2))
  }

}

object GraphicOps extends GraphicOps
