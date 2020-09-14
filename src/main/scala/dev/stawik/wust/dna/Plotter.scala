package dev.stawik.wust.dna

import java.io.File

import com.cibo.evilplot.colors.RGBA
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.LinePlot
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

object Plotter {
  val ts: () => String = () => System.currentTimeMillis().toString

  def makeTrajectories(title: String, path: String, data: Map[String, Seq[Double]]): Unit = {
    val fileName = new File(path + ts() + ".png")
    val actualPoints: Seq[Point] = Seq(Point(1, 1), Point(2, 3), Point(4, 10))
    LinePlot.series(actualPoints, "Actual Value", RGBA(20, 20, 200, 0.5))
      .frame()
      .xGrid()
      .yGrid()
      .xAxis()
      .xLabel("Steps")
      .yLabel("Value")
      .yAxis()
      .rightLegend()
      .title(title, Some(50.0))
      .render(Extent(1000, 1000))
      .write(fileName)
  }
}
