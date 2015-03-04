package utils

import breeze.linalg.DenseVector

trait Vectorable {
  def x: Double
  def y: Double
  def z: Double

  def toHomogeneousCoord: DenseVector[Double] = DenseVector(x, y, z, 1.0)

  def magnitude: Double = math.sqrt(x*x + y*y + z*z)
}

case class VRP(x: Double, y: Double, z: Double) extends Vectorable

case class VPN(x: Double, y: Double, z: Double) extends Vectorable

case class VUP(x: Double, y: Double, z: Double) extends Vectorable

case class PRP(x: Double, y: Double, z: Double) extends Vectorable

case class Vertex(x: Double, y: Double, z: Double) extends Vectorable {

  def mapToViewport(implicit w: Window, v: Viewport): Vertex = Vertex(
    x = (v.maxX - v.minX) / (w.maxX - w.minX) * (x - w.minX) + v.minX,
    y = (v.maxY - v.minY) / (w.maxY - w.minY) * (w.maxY - y) + v.minY,
    z = 0
  )
}