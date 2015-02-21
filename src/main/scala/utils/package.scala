// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

import breeze.linalg._

package object utils extends AnyRef with Parser with GraphicOps {

  case class Vertex(x: Double, y: Double, z: Double) {
    def mapToViewport(implicit w: Window, v: Viewport): Vertex = Vertex(
      x = (v.maxX - v.minX) / (w.maxX - w.minX) * (x - w.minX) + v.minX,
      y = (v.maxY - v.minY) / (w.maxY - w.minY) * (w.maxY - y) + v.minY,
      z = 0
    )

    def transform(matrix: DenseMatrix[Double]) = (matrix * toHomogeneousCoord).toVertex

    def toHomogeneousCoord: DenseVector[Double] = DenseVector(x, y, z, 1.0)
  }

  case class Face(k: Int, l: Int, m: Int)

  case class Window(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class Viewport(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class ViewVolume(minU: Int, maxU: Int, minV: Int, maxV: Int, minN: Int, maxN: Int)

  case class VRP(x: Int, y: Int, z: Int)

  case class VPN(x: Int, y: Int, z: Int)

  case class VUP(x: Int, y: Int, z: Int)

  case class PRP(x: Int, y: Int, z: Int)

  case class Rotation(degree: Double, steps: Int, axis: String)

  case class Scaling(x: Double, y: Double, z: Double, steps: Int)

  case class Translation(dx: Double, dy: Double, dz: Double, steps: Int)
}


