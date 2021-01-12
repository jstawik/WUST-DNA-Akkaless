package dev.stawik.wust.dna

import java.io.File

import dev.stawik.wust.dna.ConfigReader.Config
import dev.stawik.wust.dna.network.node.JoinersLeavers.{joinersLeaversFactory, JoinersLeaversParams}
import dev.stawik.wust.dna.network.Grid.{gridFactory, GridParams}
import dev.stawik.wust.dna.network.node.ApproxHistograms.{approxHistogramsFactory, ApproxHistogramsParams}
import dev.stawik.wust.dna.network.GridClique.{gridQFactory, GridQParams}
import dev.stawik.wust.dna.network.PBCGrid.pbcGridFactory

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.mutable.BitSet
object Simulator extends App {
  var configPath: String = "."
  if (args.length != 0 ){
    configPath = "./" + args(0)
  }
  val configFiles = new File(configPath).listFiles
  if(configFiles == null){
    println("No config files found")
  } else configFiles
    .filter(_.isFile)
    .filter(_.getName.endsWith(".json"))
    .map(new ConfigReader(_)).map(_.config).collect{case Right(config) => config}.map(executeConfig)

  def executeConfig(config: Config): Unit = {
    println(s"Executing job: $config.")
    val startTime = System.nanoTime
    val nodeFactory = config.nodeType match {
      case "JoinersLeavers" => joinersLeaversFactory(config.nodeParams.asInstanceOf[JoinersLeaversParams])
      case "ApproxHistograms" => approxHistogramsFactory(config.nodeParams.asInstanceOf[ApproxHistogramsParams])
    }
    val networkFactory = config.networkShape match {
      case "Grid" =>  gridFactory(config.networkParams.asInstanceOf[GridParams], nodeFactory)
      case "GridClique" => gridQFactory(config.networkParams.asInstanceOf[GridQParams], nodeFactory)
      case "PBCGrid" => pbcGridFactory(config.networkParams.asInstanceOf[GridParams], nodeFactory)
    }

    val itersDone = BitSet.empty
    val lineRemove = "\u001B[A\r\u001B[2K"
    println(s"Iterations done: 0/${config.iterations}")
    val results = (0 to config.iterations).par.map { iter =>
      var reports = Seq.empty[Map[String, Double]]
      val network = networkFactory()
      //network.autoStep()
      for(_ <- 0 until config.steps){
        network.updateValues(config.scale)
        network.autoStep()
        reports = reports appended network.report()
      }
      itersDone += iter
      println(lineRemove + s"Iterations done: ${itersDone.size}/${config.iterations}, time elapsed: ${(System.nanoTime - startTime)/1e9d}")
      reports
    }.toSeq
    println(s" Finished after: ${(System.nanoTime - startTime)/1e9d}")
    ResultWriter.saveJSON(config.toString, results)
  }
}
