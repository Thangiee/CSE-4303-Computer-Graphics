package utils

import breeze.linalg.DenseVector

trait Vectorable {
  def x: Double
  def y: Double
  def z: Double

  def toHomogeneousCoord: DenseVector[Double] = DenseVector(x, y, z, 1.0)
}

case class VRP(x: Double, y: Double, z: Double) extends Vectorable

case class VPN(x: Double, y: Double, z: Double) extends Vectorable

case class VUP(x: Double, y: Double, z: Double) extends Vectorable

case class PRP(x: Double, y: Double, z: Double) extends Vectorable

case class Vertex(x: Double, y: Double, z: Double) extends Vectorable {

  def mapToViewport(w: Double, h: Double)(implicit vv: ViewVolume, v: Viewport): Vertex = Vertex(
    x = ((x - vv.minU) * ((w * v.maxX - w * v.minX) / (vv.maxU - vv.minU))) + (w * v.minX),
    y = (h * v.maxY) - (y - vv.minV) * ((h * v.maxY - h * v.minY) / (vv.maxV - vv.minV)),
    z = 0
  )
}