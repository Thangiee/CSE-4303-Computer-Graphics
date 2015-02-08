// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package utils

trait Parser {

  def parseVertex(text: String): Vertex = {
    val Array(x, y, z) = text.split("\\s+").tail
    Vertex(x.toDouble, y.toDouble, z.toDouble)
  }

  def parseFace(text: String): Face = {
    val Array(k, l, m, _*) = text.split("\\s+").tail
    Face(k.toInt, l.toInt, m.toInt)
  }

  def parseWindow(text: String): Window = {
    val Array(minX, minY, maxX, maxY) = text.split("\\s+").tail
    Window(minX.toDouble, minY.toDouble, maxX.toDouble, maxY.toDouble)
  }

  def parseViewport(text: String): Viewport = {
    val Array(minX, minY, maxX, maxY) = text.split("\\s+").tail
    Viewport(minX.toDouble, minY.toDouble, maxX.toDouble, maxY.toDouble)
  }
}

object Parser extends Parser
