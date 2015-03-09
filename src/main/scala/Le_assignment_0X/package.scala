import breeze.linalg.DenseMatrix
import utils._

package object Le_assignment_0X extends AnyRef {

  case class Face(k: Int, l: Int, m: Int)

  case class Window(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class Viewport(minX: Double, minY: Double, maxX: Double, maxY: Double)

  case class ViewVolume(minU: Double, maxU: Double, minV: Double, maxV: Double, minN: Double, maxN: Double) {
    def getCenterWindow = CenterWindow((maxU + minU) / 2.0, (maxV + minV) / 2.0)
  }

  case class CenterWindow(x: Double, y: Double)

  case class Camera(
    var name: String = "",
    var projType: Projection = Projection.Parallel,
    var vrp: VRP = VRP(0, 0, 0),
    var vpn: VPN = VPN(0, 0, 1),
    var vup: VUP = VUP(0, 1, 0),
    var prp: PRP = PRP(0, 0, 1),
    var viewVolume: ViewVolume = ViewVolume(-1, 1, -1, 1, -1, 1),
    var viewport: Viewport = Viewport(.1, .1, .4, .4),
    var modelMatrix: DenseMatrix[Double] = identityMatrix
  ) {
    private val vrpOrig = vrp

    def calcModelMatrix(): Unit = {
      projType match {
        case Projection.Parallel    => modelMatrix = calcParaCompositeMatrix(this)
        case Projection.Perspective => modelMatrix = calcPerCompositeMatrix(this)
      }
    }

    def reset(): Unit = {
      vrp = vrpOrig
    }

    override def toString: String =
      s"""
        |$name
        |$modelMatrix
        |$projType
        |$vrp
        |$vpn
        |$vup
        |$prp
        |$viewVolume
        |$viewport
      """.stripMargin
  }

  sealed abstract class Projection
  object Projection {
    case object Parallel extends Projection
    case object Perspective extends Projection
  }

}
