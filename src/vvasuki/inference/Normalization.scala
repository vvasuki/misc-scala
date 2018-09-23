package vvasuki.inference
import scala.collection.mutable.ArrayBuffer
import com.weiglewilczek.slf4s.Logging
import scala.collection.mutable.Buffer

class Normalization[T <: Buffer[Double]](X: Buffer[T]) extends Logging{
  val numInputs = X.length
  val dimensions = X.head.length

  val means = (0 until dimensions).map(i => X.filter(_.length >= i + 1).map(_(i)))
    .map(statistics.getMean)
  val stdDeviations = ((0 until dimensions).map(i => X.filter(_.length >= i + 1).map(_(i)))
    zip means).
    map(x => statistics.getStdDev(x._1, x._2))

  val normalizedVectors = X.map(normalizeVector)

  def normalizeVector(v: Buffer[Double]) = (0 until v.length).
      map(i => statistics.normalize(v(i), means(i), stdDeviations(i))).toBuffer

  def denormalizeVector(v: ArrayBuffer[Double]) = (0 until v.length).
    map(i => statistics.denormalize(v(i), means(i), stdDeviations(i))).toBuffer

  override def toString = "means : " + means.toString() + " \nstdDev : " + stdDeviations.toString()

}

object statistics {
  def getMean(v: Buffer[Double]) = v.sum / v.length

  def getVariance(v: Buffer[Double], mean: Double) = v.map(_ - mean).map(x => x * x).sum / v.length

  def getStdDev(v: Buffer[Double], mean: Double) = math.sqrt(getVariance(v, mean))

  def normalize(v: Double, mean: Double, stdDev: Double) = (v - mean) / stdDev
  def denormalize(v: Double, mean: Double, stdDev: Double) = v * stdDev + mean

}

object normalizationTest extends Logging {
  def test = {
    val X = ArrayBuffer(ArrayBuffer(1d, 3, 5), ArrayBuffer(2, 4, 6d))
    val normalizationX = new Normalization(X)
    logger info normalizationX.normalizedVectors.toString()
    logger info normalizationX.means.toString()
    logger info normalizationX.stdDeviations.toString()
  }
}