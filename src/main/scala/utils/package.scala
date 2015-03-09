import java.text.DecimalFormat

import Le_assignment_0X._
import breeze.linalg.DenseVector

// Le, Thang
// 1000-787-155
// 2015-02-08
// Assignment_01

package object utils extends AnyRef with Parser with GraphicOps with Clipper {

  case class Rotation(degree: Double, steps: Int, axis: String)

  case class Scaling(x: Double, y: Double, z: Double, steps: Int)

  case class Translation(dx: Double, dy: Double, dz: Double, steps: Int)

  case class Fly(vrp1: VRP, vrp2: VRP, steps: Int)

  def less(a: Double, b:Double) = if (a > b) b else a

  implicit class RichDouble(`val`: Double) {
    def between(low: Double, high: Double) = `val` >= low && `val` <= high

    def roundTo(DecimalPlace: Int): Double = {
      if (`val`.isNaN) return 0.0
      new DecimalFormat("###." + ("#" * DecimalPlace)).format(`val`).toDouble
    }
  }

  implicit class RichDenseVector(vector: DenseVector[Double]) {
    def toVertex: Vertex = Vertex(vector(0), vector(1), vector(2))
    def toVPN = VPN(vector(0), vector(1), vector(2))
    def toVUP = VUP(vector(0), vector(1), vector(2))
    def toPRP = PRP(vector(0), vector(1), vector(2))
    def toVRP = VRP(vector(0), vector(1), vector(2))
  }
}


