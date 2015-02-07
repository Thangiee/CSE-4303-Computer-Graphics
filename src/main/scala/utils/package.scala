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

}


