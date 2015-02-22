// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

import breeze.linalg._

package object utils extends AnyRef with Parser with GraphicOps {

  case class Vertex(x: Double, y: Double, z: Double) extends Vectorable {
    def mapToViewport(implicit w: ViewVolume, v: Viewport): Vertex = Vertex(
      x = (v.maxX - v.minX) / (w.maxU - w.minU) * (x - w.minU) + v.minX,
      y = (v.maxY - v.minY) / (w.maxV - w.minV) * (w.maxV - y) + v.minY,
      z = 0
    )

    def transform(matrix: DenseMatrix[Double]) = (matrix * toHomogeneousCoord).toVertex
  }

  case class Face(k: Int, l: Int, m: Int)

  case class Window(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class Viewport(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class ViewVolume(minU: Double, maxU: Double, minV: Double, maxV: Double, minN: Double, maxN: Double) {
    def getCenterWindow = CenterWindow((maxU + minU) / 2, (maxV + minV) / 2, (maxN + minN) / 2)
  }

  case class CenterWindow(u: Double, v: Double, n: Double)

  case class VRP(x: Double, y: Double, z: Double) extends Vectorable

  case class VPN(x: Double, y: Double, z: Double) extends Vectorable

  case class VUP(x: Double, y: Double, z: Double) extends Vectorable

  case class PRP(x: Double, y: Double, z: Double) extends Vectorable

  case class Rotation(degree: Double, steps: Int, axis: String)

  case class Scaling(x: Double, y: Double, z: Double, steps: Int)

  case class Translation(dx: Double, dy: Double, dz: Double, steps: Int)
}


