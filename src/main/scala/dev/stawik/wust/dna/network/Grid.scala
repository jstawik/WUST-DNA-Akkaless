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
  def gridFactory[T <: Node](params: GridParams, newNode: String => T): () => Grid[T] = () => new Grid(params, newNode)
}
class Grid[T <: Node](params: GridParams, newNode: String => T) extends Network[T](newNode){
  lazy val edgeNode = nodes(GridLoc(0,0))
  val nodes = (for { x <- 0 until params.sideA
                     y <- 0 until params.sideB} yield GridLoc(x, y) -> newNode(s"$x, $y")).toMap

  def isValid(x: Int, y: Int, o: (Int, Int)): Boolean = {
    val xo = x + o._1
    val yo = y + o._2
    0 <= xo && xo < params.sideA && 0 <= yo && yo < params.sideB
  }
  for { x <- 0 until params.sideA
        y <- 0 until params.sideB
        offset <- Seq((-1, 0), (1, 0), (0, -1), (0, 1)) if isValid(x, y, offset)
      } yield {
        nodes(GridLoc(x, y)).receiveNeighbour(nodes(GridLoc(x+offset._1, y+offset._2)))
      }
  init()
}
