package u07.simulation.facade

import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
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
    dataset: XYSeriesCollection = new XYSeriesCollection()
  ): LineChart2D =
    LineChart2D(title, xAxisTitle, yAxisTitle, dataset)

/**
 * Line chart as a [[JPanel]] using the [[org.jfree]] chart library.
 * @param title
 *   title of the chart
 * @param xAxisTitle
 *   label of the x-axis
 * @param yAxisTitle
 *   label of the y-axis
 * @param dataset
 *   initial dataset to display in the chart
 */
class LineChart2D private (
  title: String,
  xAxisTitle: String,
  yAxisTitle: String,
  dataset: XYSeriesCollection
):
  private val lineThickness = 3.0f
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
        setStroke()

  /**
   * Sets the stroke for the lines inside the chart
   */
  private def setStroke(): Unit =
    Try:
      val renderer = chart.getPlot.asInstanceOf[XYPlot].getRenderer.asInstanceOf[XYLineAndShapeRenderer]
      val stroke = new BasicStroke(lineThickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
      (0 until dataset.getSeries.size()).foreach:
        renderer.setSeriesStroke(_, stroke)

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

    setStroke()
    chart
