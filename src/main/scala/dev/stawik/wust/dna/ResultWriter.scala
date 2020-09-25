package dev.stawik.wust.dna

import java.io.{File, PrintWriter}

import io.circe.syntax.EncoderOps

import scala.collection.parallel.immutable.ParSeq

object ResultWriter {

  def saveJSON(path: String, data: ParSeq[Seq[Map[String, Double]]]): Unit = {
    val writer= new PrintWriter(new File(s"${path}_${System.currentTimeMillis().toString}.json"))
    writer.write(data.toArray.asJson.noSpaces)
    writer.close()
  }

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
}
