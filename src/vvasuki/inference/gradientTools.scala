package vvasuki.inference
import com.weiglewilczek.slf4s.Logging
import scala.util.Random
import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.Buffer

object gradientTools extends Logging {
  def getNumericalGradient(fn: Double => Double, x: Double): Double = {
    val dx = x / 1000
    (fn(x + dx) - fn(x)) / dx
  }

  def getNumericalGradient(fn: Buffer[Double] => Double, x: Buffer[Double]): Buffer[Double] = {
    x.indices.map(idx => {
      val fnScalar = (t: Double) => {
        // Avoid creating a new array.
        val oldVal = x(idx)
        x(idx) = t
        val value = fn(x)
        x(idx) = oldVal
        value
      }
      getNumericalGradient(fnScalar, x(idx))
    }).toBuffer
  }


}