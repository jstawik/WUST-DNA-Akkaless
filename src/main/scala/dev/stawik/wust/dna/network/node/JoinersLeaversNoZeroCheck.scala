package dev.stawik.wust.dna.network.node

import dev.stawik.wust.dna.network.node.ApproxHistograms.ApproxHistogramsParams

import scala.collection.mutable

object JoinersLeaversNoZeroCheck{
  def joinersLeaversNoZeroCheckFactory(params: ApproxHistogramsParams): () => JoinersLeaversNoZeroCheck = () =>  new JoinersLeaversNoZeroCheck(params)
}

class JoinersLeaversNoZeroCheck(params: ApproxHistogramsParams) extends JoinersLeavers(params) {
  override val ApproxHistogramsParams(intervals, variables) = params
  val neighboursOverriden: mutable.Set[JoinersLeaversNoZeroCheck] = mutable.Set.empty[JoinersLeaversNoZeroCheck] //american notation is intentional
  override def receiveNeighbour(neighbour: Node): Unit = {
    neighboursOverriden.add(neighbour.asInstanceOf[JoinersLeaversNoZeroCheck])
  }
  override def result(): Double = {
    val S: Array[Double] = joiners.map(_.sum)
    val Sd: Array[Double] = leavers.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val Hd: Array[Double] = Sd.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield (minValue + intervalWidth()*(i - 1/2))*(H(i)-Hd(i))).sum
    val S2 = H.sum - Hd.sum
    S1/S2
  }
}
