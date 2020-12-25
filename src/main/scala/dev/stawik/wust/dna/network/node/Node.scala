package dev.stawik.wust.dna.network.node
import scala.collection.mutable

trait Node {
  // knowledge about the Network
  var minValue: Double = Double.NaN
  var maxValue: Double = Double.NaN
  val range: () => Double = () => maxValue - minValue
  val neighbours: mutable.Set[_ <: Node]

  // internal state
  var value: Double = Double.NaN
  var energySpent: Double = 0
  var binsChanged: Double = 0
  val done: () => Boolean

  // functionality
  def receiveNeighbour(neighbour: Node): Unit
  def initialize(minValue: Double, maxValue: Double): Unit
  def update(value: Double): Unit
  def step(): Unit
  def finalizeStep(): Unit

  def result(): Double
  def report(nodeSpecificResults: Iterable[Double], nodeCount: Int): Map[String, Double]
  def nodeSpecificResult(): Double
}
