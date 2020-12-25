package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.Network.Loc

import scala.collection.mutable
import scala.util.Random

object Network{
  trait Loc
}

abstract class Network[T <: Node](val newNode: String => T) {
  val value: () => Double = () => Random.nextDouble()
  val nodes: Map[Loc, T]
  val edgeNode: T
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
  ) ++ edgeNode.report(nodes.values.map(_.nodeSpecificResult()), nodes.size)

  def toGraphML: String = {
    val head = 
    """<?xml version="1.0" encoding="UTF-8"?>
    |<graphml xmlns="http://graphml.graphdrawing.org/xmlns"  
    |  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    |  xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
    |  http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
    |<graph id="G" edgedefault="directed">
    """.stripMargin
    val vertices = (for(node <- nodes.values) yield s"""<node id="$node" />""").fold("")(_++_)
    val edges = (for(node <- nodes.values;
                     neighbour <- node.neighbours) yield s"""<edge source="$node" target="$neighbour"/>""").fold("")(_++_)

    val foot = "</graph></graphml>"
    head++vertices++edges++foot
  }
}

