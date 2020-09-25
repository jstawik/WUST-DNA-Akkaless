package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.Grid.{GridLoc, GridParams}
import dev.stawik.wust.dna.network.Network.Loc
import dev.stawik.wust.dna.ConfigReader.NetworkParams
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object Grid {
  case class GridLoc(a: Int, b: Int) extends Loc
  case class GridParams(sideA: Int, sideB: Int) extends NetworkParams
  object GridParams{
    implicit val dec: Decoder[GridParams] = deriveDecoder
  }
}
class Grid[T <: Node](params: GridParams, newNodes: () => T) extends Network[T](newNodes){
  for(x <- 0 until params.sideA){
    for(y <- 0 until params.sideB){
      nodes += (GridLoc(x, y) -> newNode())
    }
  }
  for(x <- 0 until params.sideA) {
    for (y <- 0 until params.sideB) {
      for { i <- Seq(x-1, x+1) if i >= 0 && i < params.sideA
            j <- Seq(y-1, y+1) if j >= 0 && j < params.sideB
            } yield  {nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(x, j)))
                      nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(i, y)))}
    }
  }
  init()
}
