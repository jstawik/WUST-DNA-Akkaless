package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.Network.Loc

import scala.collection.mutable
import scala.util.Random

object Network{
  trait Loc
}

abstract class Network[T <: Node](val newNode: () => T) {
  val value: () => Double = () => Random.nextDouble()
  val nodes = mutable.Map.empty[Loc, T]
  var minValue: Double = Double.MaxValue
  var maxValue: Double = Double.MinValue

  def init(): Unit = {
    def newValue(): Double = {
      val newValue: Double = value()
      minValue = minValue.min(newValue)
      maxValue = maxValue.max(newValue)
      newValue
    }
    nodes.values.foreach(_.value = newValue())
    nodes.values.foreach(_.initialize(minValue, maxValue))
  }
  def updateValues(scale: Double): Unit = {
    def newValue(oldValue: Double): Double = {
      val newValue: Double = oldValue + Random.nextGaussian() * scale
      newValue.max(minValue).min(maxValue)
    }
    nodes.values.foreach(n => n.update(newValue(n.value)))
  }

  def step(): Unit = {
    nodes.values.foreach(_.step())
    nodes.values.foreach(_.finalizeStep())
  }
  def autoStep(): Unit = {
    while(nodes.values.exists(_.done() == false)) step()
  }
  def report(): Map[String, Double] = Map(
    "Actual" -> nodes.values.map(_.value).sum/nodes.size
  , "Max" -> nodes.values.map(_.value).max
  , "Min" -> nodes.values.map(_.value).min
  , "EnergyMin" -> nodes.values.map(_.energySpent).min
  , "EnergyMax" -> nodes.values.map(_.energySpent).max
  , "EnergyMean" -> nodes.values.map(_.energySpent).sum/nodes.size
  , "BinsChangedAvg" -> nodes.values.map(_.binsChanged).sum/nodes.size
  ) ++ nodes.values.head.report(nodes.values.map(_.nodeSpecificResult()), nodes.size)
}

