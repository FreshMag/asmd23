package u07.simulation.facade

import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import org.jfree.chart.renderer.category.{AbstractCategoryItemRenderer, LineAndShapeRenderer, StatisticalBarRenderer}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import java.awt.{BasicStroke, Color}
import javax.swing.JPanel
import scala.util.Try

/**
 * Object to help with the construction of a [[LineChart2D]]
 */
object LineChart2D:

  /**
   * Creates a new empty chart
   * @param title
   *   title of the chart
   * @param xAxisTitle
   *   label of the x-axis
   * @param yAxisTitle
   *   label of the y-axis
   * @return
   *   an empty [[LineChart2D]]
   */
  def create(
    title: String = "",
    xAxisTitle: String = "",
    yAxisTitle: String = "",
    rowLabels: Iterable[String] = Seq()
  ): LineChart2D =
    LineChart2D(title, xAxisTitle, yAxisTitle, rowLabels)

/**
 * Line chart as a [[JPanel]] using the [[org.jfree]] chart library.
 * @param title
 *   title of the chart
 * @param xAxisTitle
 *   label of the x-axis
 * @param yAxisTitle
 *   label of the y-axis
 */
class LineChart2D private (title: String, xAxisTitle: String, yAxisTitle: String, rowLabels: Iterable[String]):
  private val lineThickness = 4.0f

  private val dataset = new XYSeriesCollection()
  private val chart = createChart
  private val panel = new ChartPanel(chart, false)
  panel.setFillZoomRectangle(true)
  panel.setMouseWheelEnabled(false)

  /**
   * Returns this chart as a JPanel
   * @return
   *   a JPanel containing this chart
   */
  def asJPanel: JPanel = panel

  /**
   * Adds a value to this chart
   * @param x
   *   value on the x-axis (column of the category chart)
   * @param y
   *   value on the y-axis
   * @param row
   *   key of the row to which this value belongs to.
   */
  def addValue(x: Double, y: Double, row: Comparable[?]): Unit =
    this.synchronized:
      Try(dataset.getSeries(row).add(x, y)).getOrElse:
        val series = new XYSeries(row)
        series.add(x, y)
        dataset.addSeries(series)

  /**
   * Creates a line chart.
   * @return
   *   A line chart.
   */
  private def createChart: JFreeChart =
    val chart = ChartFactory.createXYLineChart(title, xAxisTitle, yAxisTitle, dataset)
    chart.setBackgroundPaint(Color.WHITE)

    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.setBackgroundPaint(Color.LIGHT_GRAY)
    plot.setDomainGridlinePaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.WHITE)
    plot.setAxisOffset(new RectangleInsets(1.0, 1.0, 1.0, 1.0))
    plot.setDomainCrosshairVisible(true)
    plot.setRangeCrosshairVisible(true)

    val renderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]

    val stroke = new BasicStroke(lineThickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
    (0 until rowLabels.size).foreach:
      renderer.setSeriesStroke(_, stroke)

    chart
