package dev.stawik.wust.dna.network.node

import dev.stawik.wust.dna.ConfigReader.NodeParams
import dev.stawik.wust.dna.network.node.ApproxHistograms.ApproxHistogramsParams
import io.circe.generic.semiauto.deriveDecoder
import io.circe.Decoder

import scala.collection.mutable
import scala.util.Random

object ApproxHistograms{
  case class ApproxHistogramsParams(intervals: Int, variables: Int) extends NodeParams
  object ApproxHistogramsParams{
    implicit val dec: Decoder[ApproxHistogramsParams] = deriveDecoder
  }
  def approxHistogramsFactory(params: ApproxHistogramsParams): () => ApproxHistograms = () => new ApproxHistograms(params)
}

class ApproxHistograms(params: ApproxHistogramsParams) extends Node{
  val ApproxHistogramsParams(intervals, variables) = params
  // knowledge about the Network
  val neighbours = mutable.Set.empty[ApproxHistograms]
  val intervalWidth: () => Double = () => range()/intervals

  //internal state
  val data: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  val updatedIndices = mutable.Set.empty[Int]
  val nextIndices = mutable.Set.empty[Int]

  var individualInterval: Int = -1

  //interface
  val done: () => Boolean = () => updatedIndices.isEmpty
  def receiveNeighbour(neighbour: Node): Unit = neighbours.add(neighbour.asInstanceOf[ApproxHistograms])

  def initialize(minV: Double, maxV: Double): Unit = {
    minValue = minV
    maxValue = maxV
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    data(individualInterval).indices.foreach(data(individualInterval)(_) = -Math.log(Random.nextDouble()))
    updatedIndices += individualInterval
    binsChanged += 1
  }

  def update(v: Double): Unit = {
    value = v
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    Array.fill[Double](intervals, variables)(Double.MaxValue).copyToArray(data)
    data(individualInterval).indices.foreach(data(individualInterval)(_) = -Math.log(Random.nextDouble()))
    updatedIndices += individualInterval
    binsChanged += 1
  }

  def step(): Unit = for {neigbour <- neighbours
                          updatedIndex <- updatedIndices} {neigbour.receiveInterval(updatedIndex, data(updatedIndex))
                                                            energySpent += 1}
  def finalizeStep(): Unit = {
    updatedIndices.clear()
    updatedIndices.addAll(nextIndices)
    nextIndices.clear()
  }

  def result(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield (minValue + intervalWidth()*(i - 1/2))*H(i)).sum
    val S2 = (for(i <- H.indices) yield H(i)).sum
    S1/S2
  }

  def report(): Map[String, Double] = Map("Result" -> result())

  // internal functionality
  def receiveInterval(intervalIndex: Int, newInterval: Array[Double]): Unit ={
    if(!data(intervalIndex).sameElements(newInterval)){
     nextIndices += intervalIndex
     for(idx <- newInterval.indices) data(intervalIndex)(idx) = data(intervalIndex)(idx).min(newInterval(idx))
    }
  }
}
