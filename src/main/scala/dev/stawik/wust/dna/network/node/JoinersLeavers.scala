package dev.stawik.wust.dna.network.node

import dev.stawik.wust.dna.ConfigReader.NodeParams
import dev.stawik.wust.dna.network.node.JoinersLeavers.JoinersLeaversParams
import io.circe.generic.semiauto.deriveDecoder
import io.circe.Decoder

import scala.collection.mutable
import scala.util.Random

object JoinersLeavers{
  case class JoinersLeaversParams(intervals: Int, variables: Int, zeroCheck: Boolean) extends NodeParams
  object JoinersLeaversParams{
    implicit val dec: Decoder[JoinersLeaversParams] = deriveDecoder
  }
  def joinersLeaversFactory(params: JoinersLeaversParams): String => JoinersLeavers = (name) =>  new JoinersLeavers(params){override def toString = name}
}

class JoinersLeavers(params: JoinersLeaversParams) extends Node{
  val JoinersLeaversParams(intervals, variables, zeroCheck) = params
  // knowledge about the Network
  val neighbours = mutable.Set.empty[JoinersLeavers]
  val intervalWidth: () => Double = () => range()/intervals

  // internal state
  val joiners: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  val updatedJoiners = mutable.Set.empty[Int]
  val nextJoiners = mutable.Set.empty[Int]

  val leavers: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  val updatedLeavers = mutable.Set.empty[Int]
  val nextLeavers = mutable.Set.empty[Int]

  var individualInterval: Int = -1

  // interface
  val done: () => Boolean = () => updatedJoiners.isEmpty && updatedLeavers.isEmpty
  def receiveNeighbour(neighbour: Node): Unit = neighbours.add(neighbour.asInstanceOf[JoinersLeavers])
  def initialize(minV: Double, maxV: Double): Unit = {
    minValue = minV
    maxValue = maxV
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    joiners(individualInterval).indices.foreach(joiners(individualInterval)(_) = -Math.log(Random.nextDouble()))
    updatedJoiners += individualInterval
    binsChanged += 1
  }
  def update(v: Double): Unit = {
    value = v
    val oldInterval = individualInterval
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    if(oldInterval != individualInterval){
      val newCounter = Array.fill[Double](variables)(-Math.log(Random.nextDouble()))
      for(idx <- leavers(oldInterval).indices) leavers(oldInterval)(idx) = leavers(oldInterval)(idx).min(newCounter(idx))
      for(idx <- joiners(individualInterval).indices) joiners(individualInterval)(idx) = joiners(individualInterval)(idx).min(newCounter(idx))
      updatedJoiners += individualInterval
      updatedLeavers += oldInterval
      binsChanged += 1
    }
  }
  def step(): Unit = {
     for {neigbour <- neighbours
          updatedJoiner <- updatedJoiners} {neigbour.receiveJoiner(updatedJoiner, joiners(updatedJoiner))
                                            energySpent += 1}
     for {neigbour <- neighbours
          updatedLeaver <- updatedLeavers} {neigbour.receiveLeaver(updatedLeaver, leavers(updatedLeaver))
                                            energySpent += 1}
  }
  def finalizeStep(): Unit = {
    updatedJoiners.clear()
    updatedJoiners.addAll(nextJoiners)
    nextJoiners.clear()
    updatedLeavers.clear()
    updatedLeavers.addAll(nextLeavers)
    nextLeavers.clear()
  }
  def report(nodeSpecificResults: Iterable[Double], nodeCount: Int): Map[String, Double] = Map(
    "Result" -> result()
    , "RealHistogramMean" -> nodeSpecificResults.sum/nodeCount
    , "EnergyEdge" -> energySpent 
  )
  def nodeSpecificResult(): Double = minValue + intervalWidth() * (individualInterval - 1/2)


  // internal functionality
  def receiveJoiner(intervalIndex: Int, newInterval: Array[Double]): Unit = {
    if(!joiners(intervalIndex).sameElements(newInterval)){
      nextJoiners += intervalIndex
      for(idx <- newInterval.indices) joiners(intervalIndex)(idx) = joiners(intervalIndex)(idx).min(newInterval(idx))
    }
  }
  def receiveLeaver(intervalIndex: Int, newInterval: Array[Double]): Unit = {
    if(!leavers(intervalIndex).sameElements(newInterval)){
      nextLeavers += intervalIndex
      for(idx <- newInterval.indices) leavers(intervalIndex)(idx) = leavers(intervalIndex)(idx).min(newInterval(idx))
    }
  }
  def result(): Double = {
    val S: Array[Double] = joiners.map(_.sum)
    val Sd: Array[Double] = leavers.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val Hd: Array[Double] = Sd.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = if(zeroCheck) (for (i <- H.indices) yield (minValue + intervalWidth() * (i - 1 / 2)) * (H(i) - Hd(i)).max(0)).sum
      else (for(i <- H.indices) yield (minValue + intervalWidth()*(i - 1/2))*(H(i)-Hd(i))).sum
    val S2 = if(zeroCheck) (for (i <- H.indices) yield (H(i) - Hd(i)).max(0)).sum
      else H.sum - Hd.sum
    S1/S2
  }
}