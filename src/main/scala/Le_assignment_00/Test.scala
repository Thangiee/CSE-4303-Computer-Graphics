package Le_assignment_00

import breeze.linalg._
import utils._

object Test extends App {
  val m = DenseVector(2.0 , 1.0, 1.0, 1.0)
  println(m.zRotate(45))
}
