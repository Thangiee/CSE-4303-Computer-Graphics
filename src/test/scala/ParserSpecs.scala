import Le_assignment_0X._
import org.specs2.mock._
import org.specs2.mutable._
import utils._

class ParserSpecs extends Specification with Mockito {

  "Calling parseVertex" should {
    "return a Vertex object with the correct data points" in {
      parseVertex("v 0 0 0") must_== Vertex(0, 0, 0)
      parseVertex("v 0.4 0.4 .7") must_== Vertex(.4, .4, .7)
    }
  }

  "Calling parseFace" should {
    "return a Face object with the correct data" in {
      parseFace("f 4 2 1") must_== Face(4, 2, 1)
      parseFace("f 4 2 1 3") must_== Face(4, 2, 1)
    }
  }

  "Calling parseWindow" should {
    "return a Window object with the correct data" in {
      parseWindow("w -1.0 -1.0 1\t1") must_== Window(-1, -1, 1, 1)
      parseWindow("w -1.0 -1.0 1 \t1") must_== Window(-1, -1, 1, 1)
      parseWindow("w -1.0 -1.0 1 1") must_== Window(-1, -1, 1, 1)
    }
  }

  "Calling parseViewport" should {
    "return a Viewport object with the correct data" in {
      parseViewport("s -1.0 -1.0 1\t1") must_== Viewport(-1, -1, 1, 1)
      parseViewport("s -1.0 -1.0 1 \t1") must_== Viewport(-1, -1, 1, 1)
      parseViewport("s 0.1 0.1 0.9 0.9") must_== Viewport(.1, .1, .9, .9)
    }
  }
}
