package dev.stawik.wust.dna.network.node

import dev.stawik.wust.dna.ConfigReader.NodeParams
import dev.stawik.wust.dna.network.node.JoinersLeavers.JoinersLeaversParams
import dev.stawik.wust.dna.network.node.JoinersLeaversNoZeroCheck.JoinersLeaversNoZeroCheckParams
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

import scala.collection.mutable

object JoinersLeaversNoZeroCheck{
  case class JoinersLeaversNoZeroCheckParams(intervals: Int, variables: Int) extends NodeParams
  object JoinersLeaversNoZeroCheckParams{
    implicit val dec: Decoder[JoinersLeaversNoZeroCheckParams] = deriveDecoder
  }
  def joinersLeaversNoZeroCheckFactory(params: JoinersLeaversNoZeroCheckParams): () => JoinersLeaversNoZeroCheck = () =>  new JoinersLeaversNoZeroCheck(params)
}

class JoinersLeaversNoZeroCheck(params: JoinersLeaversNoZeroCheckParams) extends JoinersLeavers(params.asInstanceOf[JoinersLeaversParams]) {
  override val JoinersLeaversNoZeroCheckParams(intervals, variables) = params
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
