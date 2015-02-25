package Le_assignment_02

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Test {
  def randomF(i: Int): Future[(Int, Double)] = Future {
    Thread.sleep(1500)
    println("Done " + i)
    (i, math.random)
  }

  val foo = for (i ← 1 to 5) yield randomF(i)


  println(Await.result(Future.sequence(foo), Duration.Inf))
}
