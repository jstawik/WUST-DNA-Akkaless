package dev.stawik.wust.dna

import scala.collection.mutable

trait Node {
  val intervals: Int
  val variables: Int
  // knowledge about the Network
  var minValue: Double = Double.NaN
  var maxValue: Double = Double.NaN
  val range: () => Double = () => maxValue - minValue
  val intervalWidth: () => Double = () => range() / intervals
  val neighbours: mutable.Set[Node]

  // internal state
  var value: Double = Double.NaN
  val data: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  val updatedIntervals = mutable.Set.empty[Int]
  val nextIntervals = mutable.Set.empty[Int]
  var energySpent: Int = 0
  var binsChanged: Int = 0
  var individualInterval: Int = -1 //Bogus value to denote this hasn't been assigned yet. Really, a shame that Int has no NaN


  // functionality
  def updateInterval(minV: Double, maxV: Double): Unit

  def finalizeStep(): Unit

  def giveIntervals(): Unit

  def receiveInterval(intervalIndex: Int, newInterval: Array[Double]): Unit

  def receiveDelta(intervalIndex: Int, newInterval: Array[Double]): Unit = {} //TODO: VERY UGLY, VERY TEMPORARY
  def result(): Double
}
