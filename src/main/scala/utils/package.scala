// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package object utils extends AnyRef with Parser {

  case class Vertex(x: Double, y: Double, z: Double) {
    def mapToViewport(implicit w: Window, v: Viewport): Vertex = Vertex(
      x = (v.maxX - v.minX) / (w.maxX - w.minX) * (x - w.minX) + v.minX,
      y = (v.maxY - v.minY) / (w.maxY - w.minY) * (w.maxY - y) + v.minY,
      z = 0
    )

  }

  case class Face(k: Int, l: Int, m: Int)

  case class Window(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class Viewport(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class DoRotation(degree: Double, steps: Int, selectedAxis: String)

  case class DoScale()
}


