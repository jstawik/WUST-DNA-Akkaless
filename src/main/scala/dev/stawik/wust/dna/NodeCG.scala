package dev.stawik.wust.dna

import scala.collection.mutable
import scala.util.Random

class NodeCG(val intervals: Int, val variables: Int) extends Node{
  // knowledge about the dev.stawik.wust.dna.Network
  override val neighbours = mutable.Set.empty[Node]

  //internal state

  // functionality
  def updateInterval(minV: Double, maxV: Double): Unit = {
    minValue = minV
    maxValue = maxV
    Array.fill[Double](intervals, variables)(Double.MaxValue).copyToArray(data)
    individualInterval = ((value-minValue)/intervalWidth()).toInt.min(intervals-1)
    data(individualInterval).indices.foreach(data(individualInterval)(_) =  -Math.log(Random.nextDouble()))
    updatedIntervals += individualInterval
    binsChanged += 1
  }
  def finalizeStep(): Unit = {
    updatedIntervals.clear()
    updatedIntervals.addAll(nextIntervals)
    nextIntervals.clear()
  }
  def giveIntervals(): Unit = {
    for {neigbour <- neighbours
         updatedInterval <- updatedIntervals} {neigbour.receiveInterval(updatedInterval, data(updatedInterval))
                                               energySpent += 1}
  }
  def receiveInterval(intervalIndex: Int, newInterval: Array[Double]): Unit = {
    if(!data(intervalIndex).sameElements(newInterval)){
      nextIntervals += intervalIndex
      for(idx <- newInterval.indices) data(intervalIndex)(idx) = data(intervalIndex)(idx).min(newInterval(idx))
    }
  }
  def result(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield minValue + intervalWidth()*(i - 1/2)*H(i)).sum
    val S2 = H.sum
    S1/S2
  }
}