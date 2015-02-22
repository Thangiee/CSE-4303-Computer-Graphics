package utils

import breeze.linalg.DenseVector

trait Vectorable {
  def x: Double
  def y: Double
  def z: Double

  def toHomogeneousCoord: DenseVector[Double] = DenseVector(x, y, z, 1.0)
}
