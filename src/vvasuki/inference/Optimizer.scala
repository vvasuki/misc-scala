package vvasuki.inference
import com.weiglewilczek.slf4s.Logging
import scala.util.Random
import scala.collection.mutable.{ ArrayBuffer, IndexedSeq, Buffer }

class Optimizer extends Logging {
  var objectiveTrajectory = ArrayBuffer[Double]()

  def stochasticGradientDescent(inputIds: Buffer[Int], startingPt: Buffer[Double],
    gradientFn: (Buffer[Double], Int) => Buffer[Double],
    objFn: (Buffer[Double]) => Double,
    numEpochs: Int = 15,
    getRandomParameters: () => Buffer[Double]) = {
    val shuffledInputIds = Random.shuffle(inputIds).toBuffer
    var stepSize = 8.0
    var momentum = 0.1

    var x = startingPt
    var minimumPt = x
    var minimumObj = objFn(x)
    var delta_x = startingPt.map(x => 0.0)

    def updatePt(gradient: Buffer[Double], inputId: Int) = {
      // momentum * delta_x + stepSize * (-gradient)
      delta_x = (delta_x.map(_ * momentum) zip gradient.map(_ * -stepSize)) map (x => x._1 + x._2)

      // x + delta_x
      x = x zip delta_x map (x => x._1 + x._2)
      val objValue = objFn(x)
      objectiveTrajectory += objValue
      if (objValue < minimumObj) {
        minimumObj = objValue
        minimumPt = x
      }

    }

    // Returns a boolean indicating descentSuccess for each input point.
    def descend = {
      logger debug "Considering inputs " + shuffledInputIds.length
      val descentSuccess = shuffledInputIds.map(i => {
        val gradient = gradientFn(x, i)
        if (gradient.forall(_ == 0)) {
          logger debug "0 gradient at input " + i
          false
        } else {
          logger debug "non 0 gradient at input " + i
          updatePt(gradient, i)
          true
        }
      })
      logger debug "descent success " + descentSuccess.toString()
      descentSuccess
    }

    (1 to numEpochs).foreach(epoch => {
      stepSize = stepSize / epoch
      logger debug "Epoch " + epoch
      while (descend.forall(_ == false)) {
        logger warn "0 gradients for all points. Reinitializing."
        x = getRandomParameters()
      }
    })
    minimumPt
  }

  def lineSearch(objFn: (Buffer[Double]) => Double, origin: Buffer[Double], searchDirection: Buffer[Double], objPrecomputed: Double = Double.NaN, initStepSize: Double = 4) = {
    val obj = if (objPrecomputed != Double.NaN) objPrecomputed
    else objFn(origin)
    val optStep = (0 until -10 by -1).map(math.pow(2, _) * initStepSize).map(s => searchDirection.map(_ * s))
      .find(step => {
        val x = (origin zip step).map(y => y._1 + y._2)
        //logger debug obj + " to " + objFn(x)
        objFn(x) < obj
      })
    optStep.getOrElse(searchDirection.map(x => 0.0)).map(_.asInstanceOf[Double])
  }

  // In case a numerical gradient is used, in some cases we can't guarantee consistent decrease in objective. This is rare.
  def gradientDescent(startingPt: Buffer[Double], objFn: (Buffer[Double]) => Double,
    gradientFn: (Buffer[Double]) => Buffer[Double], initStepSize: Double = 4, maxIter: Int = 20000) = {
    var x = startingPt
    val minChangeInX = 1e-6
    val minChangeInObj = 1e-6
    (1 to maxIter).find(iter => {
      val obj = objFn(x)
      objectiveTrajectory += obj
      val gradient = gradientFn(x)
      val searchDirection = gradient.map(-_)
      if (iter % 50 == 0)
        logger.info("Iteration " + iter + " obj " + obj)
      if(math.abs(obj) == Double.PositiveInfinity){
	      logger debug x.toString 
	      logger debug searchDirection.toString
	      logger error "Exiting : Objective is infinity, can't do gradient descent." 
	      System.exit(1)
      }
      val step = lineSearch(objFn, x, searchDirection, obj, initStepSize)
      if (iter > 1 && objectiveTrajectory.takeRight(2)(0) - obj <= minChangeInObj) {
//      if (step.map(math.abs).max <= minChangeInX) {
        logger info "Stopping at iteration " + iter
        true
      } else {
        x = (x zip step).map(y => y._1 + y._2)
        false
      }
    })
    x
  }

}