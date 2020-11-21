package dev.stawik.wust.dna.network

import dev.stawik.wust.dna.network.node.Node
import dev.stawik.wust.dna.network.Grid.{GridLoc, GridParams}

object PBCGrid{
  def pbcGridFactory[T <: Node](params: GridParams, newNode: () => T): () => PBCGrid[T] = () => new PBCGrid(params, newNode)
}

class PBCGrid[T <: Node](params: GridParams, newNode: () => T) extends Network[T](newNode){
  for(x <- 0 until params.sideA){
    for(y <- 0 until params.sideB){
      nodes += (GridLoc(x, y) -> newNode())
    }
  }
  for(x <- 0 until params.sideA) {
    for (y <- 0 until params.sideB) {
      for { i <- Seq((x-1 + params.sideA) % params.sideA, (x+1) % params.sideA)
            j <- Seq((y-1 + params.sideB) % params.sideB, (y+1) % params.sideB)
            } yield  {nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(x, j)))
                      nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(i, y)))}
    }
  }
  init()
}
