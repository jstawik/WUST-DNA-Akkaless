package dev.stawik.wust.dna

import scala.collection.mutable
import scala.util.Random

class NodeNH(val intervals: Int, val variables: Int) extends Node{
  // knowledge about the dev.stawik.wust.dna.Network
  override val neighbours = mutable.Set.empty[Node]

  // internal state
  val dataDelta: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  val updatedDeltas = mutable.Set.empty[Int]
  val nextDeltas = mutable.Set.empty[Int]


  // functionality
  def updateInterval(minV: Double, maxV: Double): Unit = {
    minValue = minV
    maxValue = maxV
    val oldInterval: Int = individualInterval
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    if(oldInterval == -1){
      data(individualInterval).indices.foreach(data(individualInterval)(_) =  -Math.log(Random.nextDouble()))
      updatedIntervals += individualInterval
      binsChanged += 1
    }
    else if(oldInterval != individualInterval){
      val newCounter = Array.fill[Double](variables)(-Math.log(Random.nextDouble()))
      for(idx <- dataDelta(oldInterval).indices) dataDelta(oldInterval)(idx) = dataDelta(oldInterval)(idx).min(newCounter(idx))
      for(idx <- data(individualInterval).indices) data(individualInterval)(idx) = data(individualInterval)(idx).min(newCounter(idx))
      updatedIntervals += individualInterval
      updatedDeltas += oldInterval
      binsChanged += 1
    }
  }

  def finalizeStep(): Unit = {
    updatedIntervals.clear()
    updatedIntervals.addAll(nextIntervals)
    nextIntervals.clear()
    updatedDeltas.clear()
    updatedDeltas.addAll(nextDeltas)
    nextDeltas.clear()
  }
  def giveIntervals(): Unit = {
    for {neigbour <- neighbours
         updatedInterval <- updatedIntervals} {neigbour.receiveInterval(updatedInterval, data(updatedInterval))
                                               energySpent += 1}
    for {neigbour <- neighbours
         updatedDelta <- updatedDeltas} {neigbour.receiveDelta(updatedDelta, dataDelta(updatedDelta))
                                               energySpent += 1}  }

  def receiveInterval(intervalIndex: Int, newInterval: Array[Double]): Unit = {
    if(!data(intervalIndex).sameElements(newInterval)){
      nextIntervals += intervalIndex
      for(idx <- newInterval.indices) data(intervalIndex)(idx) = data(intervalIndex)(idx).min(newInterval(idx))
    }
  }
  override def receiveDelta(intervalIndex: Int, newInterval: Array[Double]): Unit = {
    if(!dataDelta(intervalIndex).sameElements(newInterval)){
      nextDeltas += intervalIndex
      for(idx <- newInterval.indices) dataDelta(intervalIndex)(idx) = dataDelta(intervalIndex)(idx).min(newInterval(idx))
    }
  }
  def result(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val Sd: Array[Double] = dataDelta.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val Hd: Array[Double] = Sd.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield minValue + intervalWidth()*(i - 1/2)*(H(i)-Hd(i))).sum
    val S2 = H.sum - Hd.sum
    S1/S2
  }
  //  def result(): Double = {
//    val S: Array[Double] = data.map(_.sum)
//    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
//    val S1 = (for(i <- H.indices) yield minValue + intervalWidth()*(i - 1/2)*H(i)).sum
//    val S2 = H.sum
//
//    val Sd: Array[Double] = dataDelta.map(n => if (n.contains(Double.MaxValue)) Double.PositiveInfinity else n.sum)
//    val Hd: Array[Double] = Sd.map(a => if(a > 0) (variables-1)/a else 0)
//    val S1d = (for (i <- Hd.indices) yield minValue + intervalWidth()*(i-1/2)*Hd(i)).sum
//    val S2d = Hd.sum
//    S1/S2 - S1d/S2d
//  }
  def debugJoiners(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield minValue + intervalWidth()*(i - 1/2)*H(i)).sum
    val S2 = H.sum
    S1/S2
  }
  def debugLeavers(): Double = {
    val Sd: Array[Double] = dataDelta.map(_.sum)
    val Hd: Array[Double] = Sd.map(a => if(a > 0) (variables-1)/a else 0)
    val S1d = (for (i <- Hd.indices) yield minValue + intervalWidth()*(i-1/2)*Hd(i)).sum
    val S2d = Hd.sum
    S1d/S2d
  }
}