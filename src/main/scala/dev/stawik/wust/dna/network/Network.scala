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
  ) ++ nodes.values.head.report()
}

//object Network {
//  case class GridPBC(a: Int, b: Int) extends Shape
//  case class GridLoc(a: Int, b: Int) extends Loc
//  case class NRegular(count: Int, n: Int) extends Shape
//  case class NRegularLoc(n: Int) extends Loc
//}
//class Network[T <: Node](val shape: Shape, val newNode: () => T){
//  shape match {
//    case NRegular(count, n) =>
//      if ((n * count % 2) != 0) throw new IllegalArgumentException("`count` * `n` can't be odd")
//      for(i <- 0 until count) nodes += NRegularLoc(i) -> newNode()
//      for{i <- 0 until count
//          j <- 1 to n/2}{
//        nodes(NRegularLoc(j)).neighbours.add(nodes(NRegularLoc(i+j%count)))
//        nodes(NRegularLoc(i+j%count)).neighbours.add(nodes(NRegularLoc(j)))
//        if(n%2 != 0) nodes(NRegularLoc(i)).neighbours.add(nodes(NRegularLoc((i + count / 2) % count)))
//      }
//    case GridPBC(a, b) =>
//      for(x <- 0 until a){
//        for(y <- 0 until b){
//          nodes += (GridLoc(x, y) -> newNode())
//        }
//      }
//      for(x <- 0 until a) {
//        for (y <- 0 until b) {
//          for { i <- Seq(x-1, x+1) if i >= 0 && i < a
//                j <- Seq(y-1, y+1) if j >= 0 && j < b
//                } {nodes(GridLoc(x, y)).neighbours.add(nodes(GridLoc(x, j)))
//            nodes(GridLoc(x, y)).neighbours.add(nodes(GridLoc(i, y)))}
//        }
//      }
//      for(x <- 0 until a){nodes(GridLoc(x, 0)).neighbours.add(nodes(GridLoc(x, b-1)))
//                          nodes(GridLoc(x, b-1)).neighbours.add(nodes(GridLoc(x, 0)))}
//      for(y <- 0 until b){nodes(GridLoc(0, y)).neighbours.add(nodes(GridLoc(a-1, y)))
//                          nodes(GridLoc(a-1, y)).neighbours.add(nodes(GridLoc(0, y)))}
//    case _ =>
//  }
//  newValues()
//
//  def newValues(): Unit = {
//    def newValue(): Double = {
//      val newValue: Double = value()
//      minValue = minValue.min(newValue)
//      maxValue = maxValue.max(newValue)
//      newValue
//    }
//    nodes.values.foreach(_.value = newValue())
//    nodes.values.foreach(_.updateInterval(minValue, maxValue))
//  }
//  def updateValues(scale: Double): Unit = {
//    def newValue(oldValue: Double): Double = {
//      val newValue: Double = oldValue + Random.nextGaussian()*scale
//      newValue.max(minValue).min(maxValue)
//    }
//    nodes.values.foreach(n => n.value = newValue(n.value))
//    nodes.values.foreach(_.updateInterval(minValue, maxValue))
//  }
//  def step(): Unit = {
//    nodes.values.foreach(_.giveIntervals())
//    nodes.values.foreach(_.finalizeStep())
//  }
//  def autoStep(): Unit = {
//    while(nodes.values.exists(_.updatedIntervals.nonEmpty)) step()
//  }
//  def report(): Map[String, Double] = {
//    Map("Actual" -> nodes.values.map(_.value).sum/nodes.size
//      , "Max" -> nodes.values.map(_.value).max
//      , "Min" -> nodes.values.map(_.value).min
//      , "Result" -> nodes.values.head.result()
//      , "EnergyMin" -> nodes.values.map(_.energySpent).min
//      , "EnergyMax" -> nodes.values.map(_.energySpent).max
//      , "EnergyMean" -> nodes.values.map(_.energySpent).sum/nodes.size
//      , "BinsChangedAvg" -> nodes.values.map(_.binsChanged).sum/nodes.size
//      , "S2" -> nodes.values.head.S2()
//      , "Sd2" -> nodes.values.head.Sd2()
//    )
//  }
//}
//
//