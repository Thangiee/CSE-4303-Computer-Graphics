package utils

/**
 * Implementation of Cohen–Sutherland clipping algorithm
 */
trait Clipper {
  val Above  = 32 // 100000
  val Below  = 16 // 010000
  val Right  = 8  // 001000
  val Left   = 4  // 000100
  val Front  = 2  // 000010
  val Behind = 1  // 000001
  val Inside = 0  // 000000

  def clipLine(vertex0: Vertex, vertex1: Vertex, vv: ViewVolume): Option[(Vertex, Vertex)] = {

    def intersection(yBound: Double, x0: Double, x1: Double, y0: Double, y1: Double) = {
      x0 + (yBound - y0) * (x1 - x0) / (y1 - y0)
    }

    var v0 = vertex0
    var v1 = vertex1
    var outCode0 = getBitCode(v0, vv)
    var outCode1 = getBitCode(v1, vv)

    while (true) {
      if ((outCode0 | outCode1) == Inside) {        // trivially accept
        return Some((v0, v1))
      } else if ((outCode0 & outCode1) != Inside) { // trivially reject
        return None
      } else { // not accept or reject; do clipping

        // pick a point that is outside (at least 1 point has to be outside)
        val outCode = if (outCode0 != Inside) outCode0 else outCode1

        // find intersection point (x,y,z)
        var (x, y, z) = (0.0, 0.0, 0.0)
        if ((outCode & Above) != 0) {
          x = intersection(vv.maxV, v0.x, v1.x, v0.y, v1.y)
          y = vv.maxV
          z = intersection(vv.maxV, v0.z, v1.z, v0.y, v1.y)
        } else if ((outCode & Below) != 0) {
          x = intersection(vv.minV, v0.x, v1.x, v0.y, v1.y)
          y = vv.minV
          z = intersection(vv.minV, v0.z, v1.z, v0.y, v1.y)
        } else if ((outCode & Right) != 0) {
          x = vv.maxU
          y = intersection(vv.maxU, v0.y, v1.y, v0.x, v1.x)
          z = intersection(vv.maxU, v0.z, v1.z, v0.x, v1.x)
        } else if ((outCode & Left) != 0) {
          x = vv.minU
          y = intersection(vv.minU, v0.y, v1.y, v0.x, v1.x)
          z = intersection(vv.minU, v0.z, v1.z, v0.x, v1.x)
        } else if ((outCode & Front) != 0) {
          x = intersection(vv.maxN, v0.x, v1.x, v0.z, v1.z)
          y = intersection(vv.maxN, v0.y, v1.y, v0.z, v1.z)
          z = vv.maxN
        } else if ((outCode & Behind) != 0) {
          x = intersection(vv.minN, v0.x, v1.x, v0.z, v1.z)
          y = intersection(vv.minN, v0.y, v1.y, v0.z, v1.z)
          z = vv.minN
        }

        // set the new clipped point and get ready to check the other endpoint
        if (outCode == outCode0) {
          v0 = Vertex(x, y, z)
          outCode0 = getBitCode(v0, vv)
        } else {
          v1 = Vertex(x, y, z)
          outCode1 = getBitCode(v1, vv)
        }
      }
    }

    Some((v0, v1))
  }

  def getBitCode(v: Vertex, vv: ViewVolume): Int = {
    var code = Inside

    if      (v.y > vv.maxV) code |= Above
    else if (v.y < vv.minV) code |= Below
    if      (v.x > vv.maxU) code |= Right
    else if (v.x < vv.minU) code |= Left
    if      (v.z > vv.maxN) code |= Front
    else if (v.z < vv.minN) code |= Behind

    code
  }

  implicit class RichDouble(`val`: Double) {
    def between(low: Double, high: Double) = `val` >= low && `val` <= high
  }
}

object Clipper extends Clipper
