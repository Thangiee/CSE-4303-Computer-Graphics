package Le_assignment_02

import utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Test extends App {
  def randomF(i: Int): Future[(Int, Double)] = Future {
    Thread.sleep(1500)
    println("Done " + i)
    (i, math.random)
  }

//  val foo = for (i ← 1 to 5) yield randomF(i)


//  println(Await.result(Future.sequence(foo), Duration.Inf))
  implicit val w = Window(-20, 2, -8, 27)
  implicit val v = Viewport(.4, .2, .7, .8)
  val x = Vertex(-4, 12, 0).mapToViewport
  println(x.x * 1920)
  println(x.y * 1080)

}
