package utils

import breeze.linalg._
import math._

trait GraphicOps {

  implicit class RichDenseVector(vector: DenseVector[Double]) {
    def zRotate(deg: Double): DenseVector[Double] = {
      val c = cos(toRadians(deg))
      val s = sin(toRadians(deg))
      val rotationMatrix = DenseMatrix(
        (  c,  -s, 0.0, 0.0),
        (  s,   c, 0.0, 0.0),
        (0.0, 0.0, 1.0, 0.0),
        (0.0, 0.0, 0.0, 1.0)
      )

      rotationMatrix * vector
    }

    def toVertex: Vertex = Vertex(vector(0), vector(1), vector(2))
  }
}

object GraphicOps extends GraphicOps
