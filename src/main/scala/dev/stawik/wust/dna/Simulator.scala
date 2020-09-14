package dev.stawik.wust.dna

import java.io.{File, PrintWriter}

import dev.stawik.wust.dna.Network.Grid
import io.circe.syntax._

import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable


object Simulator extends App {
  val intervals = 20
  val variables = 80
  val gridSide = 50
  val iterations = 240
  val steps = 600
  //val resultsCG = mutable.Set.empty[Map[String, Double]]
  //val resultsNH = mutable.Set.empty[Map[String, Double]]
  //val resultsCG: ParSeq[Map[String, Double]] = (0 until iterations).par.map(_ => {
  //    val networkCG = new Network[NodeCG](Grid(gridSide, gridSide), () => new NodeCG(intervals, variables))
  //    networkCG.autoStep()
  //    networkCG.report()
  //  }
  //)
  //saveCSV("testFile", resultsCG)
  println("Starting")
  val resultsNH: ParSeq[Seq[Map[String, Double]]] = (0 until iterations).par.map(_ => {
    var reports = Seq.empty[Map[String, Double]]
    val networkNH = new Network[NodeNH](Grid(gridSide, gridSide), () => new NodeNH(intervals, variables))
    networkNH.autoStep()
    reports = reports appended networkNH.report()
    for (_ <- 0 until steps) {
      networkNH.updateValues(.1 / intervals)
      networkNH.autoStep()
      reports = reports appended networkNH.report()
    }
    reports
  }
  )
  //println(resultsNH)
  val tmp = new PrintWriter(new File(s"$intervals-$variables-$gridSide-$iterations-$steps-${System.currentTimeMillis().toString}.json"))
  tmp.write(resultsNH.toArray.asJson.noSpaces)
  tmp.close()
  println("Done")

  //for(seq <- result) saveCSV(s"${seq.hashCode()}_sequence_of_NH", seq)
  def saveCSV(path: String, data: Seq[Map[String, Double]]): Unit = {
    val writer = new PrintWriter(new File(s"${path}_${System.currentTimeMillis().toString}.csv"))
    val header: List[String] = data.head.keys.toList
    val rowCount: Int = data.size + 1
    val content = Array.fill[String](rowCount, header.size)("")
    for (col <- header.indices) {
      val label = header(col)
      content(0)(col) = label
      val body: Seq[Double] = data.map(_ (label))
      for (row <- 1 until rowCount) content(row)(col) = body(row - 1).toString
    }
    content.foreach(
      row => {
        row.foreach { col: String => writer.write(s"$col, ") }
        writer.write("\n")
      }
    )
    writer.close()
  }


  //(0 until 30).toVector.par.map{
  //  val networkCG = new Network[NodeCG](Grid(gridSide, gridSide), () => new NodeCG(intervals, variables))
  //  networkCG.autoStep()
  //  resultsCG.add(networkCG.report())
  //}
  //(0 until 30).toVector.par.map{
  //  val networkNH = new Network[NodeNH](Grid(gridSide, gridSide), () => new NodeNH(intervals, variables))
  //  networkNH.autoStep()
  //  resultsNH.add(networkNH.report())
  //}

  //println(resultsCG.asJson.noSpaces)
  //println(resultsNH.asJson.noSpaces)
}
