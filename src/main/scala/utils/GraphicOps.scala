package utils

import Le_assignment_0X._
import breeze.linalg._

import scala.math._

trait GraphicOps {

  def identityMatrix: DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def translate(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0, dx),
      (0.0, 1.0, 0.0, dy),
      (0.0, 0.0, 1.0, dz),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def scale(sx: Double = 1.0, sy: Double = 1.0, sz: Double = 1.0): DenseMatrix[Double] = {
    DenseMatrix(
      ( sx, 0.0, 0.0, 0.0),
      (0.0,  sy, 0.0, 0.0),
      (0.0, 0.0,  sz, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def xRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg))
    val s = sin(toRadians(deg))
    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0,   c,  -s, 0.0),
      (0.0,   s,   c, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def yRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg)).roundTo(5)
    val s = sin(toRadians(deg)).roundTo(5)
    DenseMatrix(
      (  c, 0.0,   s, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      ( -s, 0.0,   c, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def zRotate(deg: Double): DenseMatrix[Double] = {
    val c = cos(toRadians(deg)).roundTo(5)
    val s = sin(toRadians(deg)).roundTo(5)
    DenseMatrix(
      (  c,  -s, 0.0, 0.0),
      (  s,   c, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def shearXZ(x: Double): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0,   x, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def shearYZ(y: Double): DenseMatrix[Double] = {
    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0,   y, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def xRotateToPlaneXZ(vpn: VPN): DenseMatrix[Double] = {
    val (x, y, z) = (vpn.x, vpn.y, vpn.z)
    val v = sqrt(y*y + z*z).roundTo(5)
    val a = if (y*y + z*z == 0) 1 else y / v
    val b = if (y*y + z*z == 0) 1 else z / v

    DenseMatrix(
      (1.0, 0.0, 0.0, 0.0),
      (0.0,   b,  -a, 0.0),
      (0.0,   a,   b, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def yRotateToAlignZ(vpn: VPN): DenseMatrix[Double] = {
    val (a, b, c) = (vpn.x, vpn.y, vpn.z)
    val d = sqrt(a*a + c*c).roundTo(5)

    DenseMatrix(
      (c/d, 0.0,-a/d, 0.0),
      (0.0, 1.0, 0.0, 0.0),
      (a/d, 0.0, c/d, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def zRotateToPlaneYZ(vup: VUP): DenseMatrix[Double] = {
    val (a, b, c) = (vup.x, vup.y, vup.z)
    val d = sqrt(a*a + b*b).roundTo(5)

    DenseMatrix(
      (b/d,-a/d, 0.0, 0.0),
      (a/d, b/d, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 1.0)
    )
  }

  def calcParaCompositeMatrix(camera: Camera): DenseMatrix[Double] = {
    var vrp = camera.vrp
    var vpn = camera.vpn
    var vup = camera.vup
    var prp = camera.prp
    val viewVol = camera.viewVolume

    // Translate VRP to Origin
    val t = translate(-vrp.x, -vrp.y, -vrp.z)
    vrp = t x vrp

    // get the matrix to rotate around x util VPN lies in the xz plane positively
    val rx = xRotateToPlaneXZ(vpn)
    vpn = rx x vpn
    vup = rx x vup

    // get the matrix to rotate around y util VPN align with the positive x-axis
    val ry = yRotateToAlignZ(vpn)
    vpn = ry x vpn
    vup = ry x vup

    // get the matrix to rotate around z util VUP lies in the yz plane positively
    val rz = zRotateToPlaneYZ(vup)
    vpn = rz x vpn
    vup = rz x vup

    // find the DOP and shear it to become parallel to the z-axis
    val cw = viewVol.getCenterWindow
    val shx = shearXZ((cw.x - prp.x) / prp.z)
    val shy = shearYZ((cw.y - prp.y) / prp.z)
    vrp = Seq(shx, shy).reduce(_ * _) x vrp
    prp = Seq(shx, shy).reduce(_ * _) x prp

    // translate center of window on the front plant to the origin
    val t2 = translate(-less(viewVol.maxU, viewVol.minU), -less(viewVol.maxV, viewVol.minV), -less(viewVol.maxN, viewVol.minN))
    vrp = t2 x vrp
    prp = t2 x prp

    // scale to canonical Viewing Volume (1 x 1 x 1)
    val sc = scale(
      sx = 1 / abs(viewVol.maxU - viewVol.minU),
      sy = 1 / abs(viewVol.maxV - viewVol.minV),
      sz = 1 / abs(viewVol.maxN - viewVol.minN)
    )
    vrp = sc x vrp
    prp = sc x prp

    Seq(sc, t2, shy, shx, rz, ry, rx, t).reduce(_ * _)
  }

  def calcPerCompositeMatrix(camera: Camera): DenseMatrix[Double] = {
    var vrp = camera.vrp
    var vpn = camera.vpn
    var vup = camera.vup
    var prp = camera.prp
    val viewVol = camera.viewVolume

    // Translate VRP to Origin
    val t = translate(-vrp.x, -vrp.y, -vrp.z)
    vrp = t x vrp

    // get the matrix to rotate around x util VPN lies in the xz plane positively
    val rx = xRotateToPlaneXZ(vpn)
    vpn = rx x vpn
    vup = rx x vup

    // get the matrix to rotate around y util VPN align with the positive x-axis
    val ry = yRotateToAlignZ(vpn)
    vpn = ry x vpn
    vup = ry x vup

    // get the matrix to rotate around z util VUP lies in the yz plane positively
    val rz = zRotateToPlaneYZ(vup)
    vup = rz x vup

    // find the matrix to translate prp to the origin, but don't do the translation
    // yet since it's easier to find the DOP before the translation
    val t2 = translate(-prp.x, -prp.y, -prp.z)

    val cw = viewVol.getCenterWindow
    val (dopX, dopY) = (cw.x - prp.x, cw.y - prp.y)

    // find the matrix to shear the view volume such that the center line becomes the z-axis
    val shx = shearXZ(dopX / prp.z)
    val shy = shearYZ(dopY / prp.z)

    prp = Seq(t2, shy, shx).reduce(_ * _) x prp
    vrp = Seq(t2, shy, shx).reduce(_ * _) x vrp

    val sc = scale(
      sx = (2 * abs(vrp.z)) / ((viewVol.maxU - viewVol.minU) * (vrp.z + viewVol.maxN)),
      sy = (2 * abs(vrp.z)) / ((viewVol.maxV - viewVol.minV) * (vrp.z + viewVol.maxN)),
      sz = 1 / (vrp.z + viewVol.maxN)
    )

    prp = sc x prp
    vrp = sc x vrp

    Seq(sc, shy, shx, t2, rz, ry, rx, t).reduce(_ * _)
  }

  implicit class RichDenseMatrix(matrix: DenseMatrix[Double]) {
    def translation(dx: Double = 0.0, dy: Double = 0.0, dz: Double = 0.0): DenseMatrix[Double] = matrix * GraphicOps.translate(dx, dy, dz)
    def scale(sx: Double = 1.0, sy: Double = 1.0, sz: Double = 1.0): DenseMatrix[Double] = matrix * GraphicOps.scale(sx, sy, sz)
    def xRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.xRotate(deg)
    def yRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.yRotate(deg)
    def zRotate(deg: Double): DenseMatrix[Double] = matrix * GraphicOps.zRotate(deg)

    def x(vpn: VPN) = (matrix * vpn.toHomogeneousCoord).toVPN
    def x(vup: VUP) = (matrix * vup.toHomogeneousCoord).toVUP
    def x(prp: PRP) = (matrix * prp.toHomogeneousCoord).toPRP
    def x(vrp: VRP) = (matrix * vrp.toHomogeneousCoord).toVRP
    def x(vertex: Vertex) = (matrix * vertex.toHomogeneousCoord).toVertex

    def x(that: DenseMatrix[Double]) = Seq(matrix, that).reduce(_ * _)
  }
}

object GraphicOps extends GraphicOps
