package utils

import Le_assignment_0X._


trait SutherlandHodgman {

  def clip(vertex0: Vertex, vertex1: Vertex, vp: Viewport, w: Double, h: Double): Option[(Vertex, Vertex)] = {
    val clipper = Seq(vp.minX, vp.maxX, vp.maxX, vp.minX).map(_ * w).zip(Seq(vp.minY, vp.minY, vp.maxY, vp.maxY).map(_ * h))
    var result = Seq((vertex0.x * w, vertex0.y * h), (vertex1.x * w, vertex1.y * h))

    val len = clipper.size
    for (i <- 0 until len) {
      val len2 = result.size
      val input = result
      result = Seq()

      val a = clipper((i + len - 1) % len)
      val b = clipper(i)

      for ( j <- 0 until len2 ) {
        val p = input((j + len2 - 1) % len2)
        val q = input(j)

        if (inside(a, b, q)) {
          if (!inside(a, b, p))
            result = result :+ intersection(a, b, p, q)
          result = result :+ q
        }
        else if (inside(a, b, p))
          result = result :+ intersection(a, b, p, q)
      }
    }

    if (result.isEmpty) {
      None
    } else {
      Some((Vertex(result(0)._1, result(0)._2, 0), Vertex(result(1)._1, result(1)._2, 0)))
    }
  }

  private def inside(a: (Double, Double), b: (Double, Double), c: (Double, Double)) =
    (a._1 - c._1) * (b._2 - c._2) > (a._2 - c._2) * (b._1 - c._1)

  private def intersection(a: (Double, Double), b: (Double, Double), p: (Double, Double), q: (Double, Double)) = {
    val a1 = b._2 - a._2
    val b1 = a._1 - b._1
    val c1 = a1 * a._1 + b1 * a._2
    val a2 = q._2 - p._2
    val b2 = p._1 - q._1
    val c2 = a2 * p._1 + b2 * p._2

    val det = a1 * b2 - a2 * b1
    ((b2 * c1 - b1 * c2) / det, (a1 * c2 - a2 * c1) / det)
  }
}

object SutherlandHodgman extends SutherlandHodgman
