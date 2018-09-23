package vvasuki.toys

import com.weiglewilczek.slf4s.Logging
import scala.collection.mutable.Buffer

object TriArrayDistance extends Logging {
  def distance(a: Double, b: Double, c: Double) = List(a, b, c).max - List(a, b, c).min
  
  
  def getMinDistance(arrA: Buffer[Double], arrB: Buffer[Double], arrC: Buffer[Double]) = {
    val arrays = Buffer(arrA.sorted, arrB.sorted, arrC.sorted)
    val pointers = Buffer(0,0,0)
    var bDone = false
    var minDistance = Double.PositiveInfinity
    def getCurrElement(arrayId: Int) = arrays(arrayId)(pointers(arrayId)) 
    while(!bDone) {
      val sortedBufferIds = (0 to 2).sortBy(i => arrays(i)(pointers(i)))
      val d = getCurrElement(sortedBufferIds.last) - getCurrElement(sortedBufferIds.head)
      if(d < minDistance) minDistance = d
      if(d == 0 || pointers(sortedBufferIds.head) == arrays(sortedBufferIds.head).length - 1) 
        bDone = true
      // Move pointer in the min-Buffer forward.
      pointers(sortedBufferIds.head) = pointers(sortedBufferIds.head) + 1
    }
    minDistance

  }
  def main(args: Array[String]) = {
    var minDist = getMinDistance(Buffer(1,2, 3.0), Buffer(1, 4, 5, 6.0), Buffer(6, 7, 8, 9.0))
    println ("min distance: " + minDist.toString)

  }
}
