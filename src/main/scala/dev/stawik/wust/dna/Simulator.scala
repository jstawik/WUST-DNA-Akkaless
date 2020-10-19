package dev.stawik.wust.dna

import java.io.File

import dev.stawik.wust.dna.ConfigReader.Config
import dev.stawik.wust.dna.network.node.JoinersLeavers.{joinersLeaversFactory, JoinersLeaversParams}
import dev.stawik.wust.dna.network.Grid
import dev.stawik.wust.dna.network.Grid.GridParams
import dev.stawik.wust.dna.network.node.ApproxHistograms.{approxHistogramsFactory, ApproxHistogramsParams}

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

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
    print(s"Executing job: $config.")
    val startTime = System.nanoTime
    val nodeFactory = config.nodeType match {
      case "JoinersLeavers" => joinersLeaversFactory(config.nodeParams.asInstanceOf[JoinersLeaversParams])
      case "ApproxHistograms" => approxHistogramsFactory(config.nodeParams.asInstanceOf[ApproxHistogramsParams])
      //case _ => println(s"${config.nodeType} not recognized as nodeType")
    }
    val results = (0 to config.iterations).par.map { _ =>
      var reports = Seq.empty[Map[String, Double]]
      val network = config.networkShape match {
        case "Grid" =>  new Grid(config.networkParams.asInstanceOf[GridParams], nodeFactory)
        //case _ => println(s"${config.networkShape} not recognized as networkShape")
      }
      network.autoStep()
      for(_ <- 0 until config.steps){
        network.updateValues(config.scale)
        network.autoStep()
        reports = reports appended network.report()
      }
      reports
    }.toSeq
    println(s" Finished after: ${(System.nanoTime - startTime)/1e9d}")
    ResultWriter.saveJSON(config.toString, results)
  }

//  //case class SimConfig(intervals: Int, variables: Int, gridSideA: Int, gridSideB: Int, iterations: Int, steps: Int)
//  //val config = fromFile("config.json").mkString.asJsonObject(EncoderOps[Seq[SimConfig]])
//  val intervals = 20
//  val variables = 50
//  val gridSide = 5
//  val iterations = 32 //14min
//  val steps = 6000
//
//  println("Starting")
//
//  //val singleRun = Future[Seq[Map[String, Double]]] {
//  //  var reports = Seq.empty[Map[String, Double]]
//  //  val networkNH = new Network[NodeNH](Grid(gridSide, gridSide), () => new NodeNH(intervals, variables))
//  //  networkNH.autoStep()
//  //  reports = reports appended networkNH.report()
//  //  for (_ <- 0 until steps) {
//  //    networkNH.updateValues(.1 / intervals)
//  //    networkNH.autoStep()
//  //    reports = reports appended networkNH.report()
//  //  }
//  //  reports
//  //}
//
//
//  val resultsNH: ParSeq[Seq[Map[String, Double]]] = (0 until iterations).par.map(_ => {
//  var reports = Seq.empty[Map[String, Double]]
//  val networkNH = new Network[JoinersLeavers](Grid(gridSide, gridSide), () => new JoinersLeavers(intervals, variables))
//  networkNH.autoStep()
//  reports = reports appended networkNH.report()
//  for (_ <- 0 until steps) {
//    networkNH.updateValues(.5 / intervals)
//    networkNH.autoStep()
//    reports = reports appended networkNH.report()
//  }
//  reports
//}
//)
//
//  println("Done")
//
}
