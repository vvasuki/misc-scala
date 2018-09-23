package vvasuki.inference

// Code inspired by https://github.com/yannart/Scala-Neural-Network/tree/master/src/main/scala/com/yannart/neuralnetwork

import scala.collection.mutable.{ Buffer, ArrayBuffer, StringBuilder }
import scala.math.{ exp, pow, random, sqrt }
import scala.xml.{ Elem, XML }
import com.weiglewilczek.slf4s.Logging

abstract class ActivationFunction extends Logging {
  def eval(value: Double): Double
  def derivative(value: Double): Double
}

object Sigmoid extends ActivationFunction {
  override def eval(value: Double): Double = {
    1d / (1d + exp(-value))
  }

  override def derivative(value: Double) = exp(-value) * eval(value) * eval(value)
}

object Neuron {
    //Use [-2.4, 2.4] as initial weight range
  def getRandomWeights(inputNum: Int) = Buffer.fill(inputNum)(random * (2.4 * 2) - 2.4)
}

class Neuron(var weights: Buffer[Double], var activationFn: ActivationFunction = Sigmoid) extends Logging {

  def this(inputNum: Int) = {
    this(Neuron.getRandomWeights(inputNum))
  }

  def output = activationFn.eval(activation)
  var input = Buffer[Double]()
  var activation = 0.0
  var backPropError = 0.0

  def run(inputIn: Buffer[Double]): Double = {
    input = inputIn
    activation = (input zip weights).map(x => x._1 * x._2).sum
    output
  }

  def getNumericalGradientDirectly(errFn: () => Double) = {
    val fn = (w: Buffer[Double]) => {
      this.weights = w.toBuffer
      errFn()
    }
    gradientTools.getNumericalGradient(fn, weights).toBuffer
  }

  def getBackPropagationError(backPropErrorsSucceeding: Buffer[Double]) = {
    val gradOutputWrtActivation = activationFn.derivative(activation)
    //TODO: Calculate in Log-space?
    backPropErrorsSucceeding.map(_ * gradOutputWrtActivation).sum
  }

  def getGradient(backPropErrorsSucceeding: Buffer[Double]) = {
    backPropError = getBackPropagationError(backPropErrorsSucceeding)
    val gradient = input.map(_ * backPropError)
    gradient
  }

  override def toString(): String = {
    weights.mkString(" ")
  }
}
class MultiLayerLogisticFn(val layers: Buffer[Buffer[Neuron]]) extends Logging {
  def outputLayer = layers.takeRight(1)(0)

  def this(numInputVars: Int, numNeuronsInLayer: Buffer[Int]) = {
    this(Buffer.fill(numNeuronsInLayer.size)(Buffer[Neuron]()))
    var numInputs = numInputVars + 1
    //Creates the layers with each the right number of neurons
    for (i <- 0 until numNeuronsInLayer.size) {
      //creates the layer
      layers(i) = Buffer.fill(numNeuronsInLayer(i))(new Neuron(numInputs))
      //the number of input of the next
      //layer is the current number of neurons
      numInputs = numNeuronsInLayer(i) + 1
    }
  }

  def run(input: Buffer[Double]): Buffer[Double] = {
    var layerInput = Buffer(1.0) ++ input
    layers.indices.foreach(i => {
      val neurons = layers(i)
      neurons.foreach(_.run(layerInput))
      layerInput = Buffer(1d) ++ neurons.map(_.output)
    })
    return layerInput.tail
  }

  def getGradient(input: Buffer[Double], errorGradientsWrtOutput: Buffer[Double]) = {
    run(input)
    var backPropErrorsSucceeding = errorGradientsWrtOutput
    (layers.size - 1 to 0 by -1).map(i => {
      logger debug backPropErrorsSucceeding.toString()
      val neurons = layers(i)
      val layerGradient = neurons.map(_.getGradient(backPropErrorsSucceeding))
      backPropErrorsSucceeding = neurons.map(_.backPropError)
      layerGradient
    }).reverse.toBuffer
  }

  def getParameters = {
    layers.map(_.map(_.weights))
  }
  
  def getRandomParameters = layers.map(_.map(n => Neuron.getRandomWeights(n.weights.length)))

  def setParameters(allWeights: Buffer[Double]) = {
    val wtIterator = allWeights.iterator
    layers.foreach(_.foreach(neuron => {
      neuron.weights = wtIterator.take(neuron.weights.length).toBuffer
    }))
  }

  def getNumericalGradientDirectly(errFn: () => Double) = {
    (layers.size - 1 to 0 by -1).map(i => {
      layers(i).map(_.getNumericalGradientDirectly(errFn))
    }).reverse.toBuffer
  }

  override def toString(): String = {
    layers.map(_.mkString(", ")).mkString("\n")
  }

}

object MultiLayerLogisticFnTest extends Logging {
  def test = {
    val input = Buffer(1.0, 2)
    val numNeurons = Buffer(2, 1)
    val neuralNet = new MultiLayerLogisticFn(2, numNeurons)
    val output = neuralNet.run(input).head
    logger debug "MultilayerLogisticFn: " + neuralNet.toString()
    logger debug "Output: " + output.toString()

    val errFn = () => {
      val x = neuralNet.run(input).head
      x * x
    }
    val errorGradientsWrtOutput = Buffer(2 * output)
    val gradients = neuralNet.getGradient(input, errorGradientsWrtOutput)
    logger debug gradients.mkString("\n")
    logger debug gradients.flatten.flatten.mkString(" ")

    val numericalGradient = neuralNet.getNumericalGradientDirectly(errFn)
    logger debug numericalGradient.mkString("\n")

    val weights = neuralNet.getParameters
    logger debug weights.toString
    neuralNet.setParameters(weights.flatten.flatten.map(_ * 2))
    logger debug neuralNet.toString()

  }

}
