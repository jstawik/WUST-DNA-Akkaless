package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.Network.Loc
import dev.stawik.wust.dna.ConfigReader.NetworkParams
import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.GridClique._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object GridClique {
  case class GridQLoc(a: Int, b: Int, i: Int) extends Loc
  case class GridQParams(sideA: Int, sideB: Int, cliqueSize: Int) extends NetworkParams
  object GridQParams{
    implicit val dec: Decoder[GridQParams] = deriveDecoder
  }
  def gridQFactory[T <: Node](params: GridQParams, newNode: String => T): () => GridClique[T] = () => new GridClique(params, newNode)
}
class GridClique[T <: Node](params: GridQParams, newNode: String => T) extends Network[T](newNode){
  lazy val edgeNode = nodes(GridQLoc(0,0,0))
  val nodes = (for { x <- 0 until params.sideB
                     y <- 0 until params.sideB
                     n <- 0 until params.cliqueSize} yield GridQLoc(x, y, n) -> newNode(s"$x, $y: $n")).toMap

  for(x <- 0 until params.sideA) {
    for(y <- 0 until params.sideB) {
      for(n <- 0 until params.cliqueSize)
        for(m <- 0 until params.cliqueSize)
          nodes(GridQLoc(x, y, n)).receiveNeighbour(nodes(GridQLoc(x, y, m)))
      for {i <- Seq((x - 1, 1), (x + 1, 3)) if i._1 >= 0 && i._1 < params.sideA
           j <- Seq((y - 1, 0), (y + 1, 2)) if j._1 >= 0 && j._1 < params.sideB
           } yield { nodes(GridQLoc(x,y, i._2)).receiveNeighbour(nodes(GridQLoc(i._1, y, (i._2 + 2) % 4)))
                     nodes(GridQLoc(x,y, j._2)).receiveNeighbour(nodes(GridQLoc(x, j._1, (j._2 + 2) % 4))) } //TODO: needs a refresh

    }
  }
  init()
}
