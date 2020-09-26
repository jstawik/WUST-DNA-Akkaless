package dev.stawik.wust.dna


import java.io.File

import scala.io.BufferedSource
import scala.io.Source.fromFile
import cats.implicits._
import dev.stawik.wust.dna.network.Grid.GridParams
import dev.stawik.wust.dna.ConfigReader.Config
import dev.stawik.wust.dna.network.node.ApproxHistograms.ApproxHistogramsParams
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode

object ConfigReader{
  trait NetworkParams
  object NetworkParams {
    implicit val dec: Decoder[NetworkParams] =
      GridParams.dec.widen[NetworkParams]
  }
  trait NodeParams
  object NodeParams {
    implicit val dec: Decoder[NodeParams] =
      ApproxHistogramsParams.dec.widen[NodeParams]
  }
  case class Config(networkShape: String, networkParams: NetworkParams, nodeType: String, nodeParams: NodeParams, steps: Int, iterations: Int, scale: Double)
  object Config {
    implicit val dec: Decoder[Config] = deriveDecoder
  }
}
class ConfigReader(path: File){
  val source: BufferedSource = fromFile(path)
  val config: Either[io.circe.Error, Config] = decode[Config](source.mkString)
  source.close()
}
