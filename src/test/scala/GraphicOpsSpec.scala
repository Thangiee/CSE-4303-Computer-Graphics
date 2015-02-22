import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import utils._

class GraphicOpsSpec extends Specification with Mockito {
  "Calling xRotateToPlaneXZ" should {
    "return a VPN with the correct rotation" in {
      val vpn = VPN(10, 5, 5)
      val m = xRotateToPlaneXZ(vpn)
      val vpn2 = (m * vpn.toHomogeneousCoord).toVPN

      vpn2.x must_== 10
      vpn2.y must_== 0
      vpn2.z must beCloseTo(7.07 +/- .01)
    }
  }

  "Calling yRotateToAlignZ" should {
    "return a VPN with the correct rotation" in {
      val vpn = VPN(10, 0, 7.07)
      val m = yRotateToAlignZ(vpn)
      val vpn2 = (m * vpn.toHomogeneousCoord).toVPN

      vpn2.x must beCloseTo(0.0 +/- .01)
      vpn2.y must beCloseTo(0.0 +/- .01)
      vpn2.z must beCloseTo(12.247 +/- .01)
    }
  }

  "Calling zRotateToPlaneYZ" should {
    "return a VUP with the correct rotation" in {
      val vup = VUP(-.577, .707, .408)
      val m = zRotateToPlaneYZ(vup)
      val vup2 = (m * vup.toHomogeneousCoord).toVUP

      vup2.x must beCloseTo(0.0 +/- .01)
      vup2.y must beCloseTo(0.913 +/- .01)
      vup2.z must beCloseTo(0.408 +/- .01)
    }
  }
}
