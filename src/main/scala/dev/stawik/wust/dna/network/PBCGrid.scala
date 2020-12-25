package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.Grid.{GridLoc, GridParams}

object PBCGrid{
  def pbcGridFactory[T <: Node](params: GridParams, newNode: (String) => T): () => PBCGrid[T] = () => new PBCGrid(params, newNode)
}

class PBCGrid[T <: Node](params: GridParams, newNode: String => T) extends Network[T](newNode){
  lazy val edgeNode = nodes(GridLoc(0,0))
  val nodes = (for { x <- 0 until params.sideB
                     y <- 0 until params.sideB} yield GridLoc(x, y) -> newNode(s"$x, $y")).toMap

  for(x <- 0 until params.sideA) {
    for (y <- 0 until params.sideB) {
      for { i <- Seq((x-1 + params.sideA) % params.sideA, (x+1) % params.sideA)
            j <- Seq((y-1 + params.sideB) % params.sideB, (y+1) % params.sideB)
            } yield  {nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(x, j)))
                      nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(i, y)))} //TODO: needs refreshing, maybe just take a grid and add neighs?
    }
  }
  init()
}
