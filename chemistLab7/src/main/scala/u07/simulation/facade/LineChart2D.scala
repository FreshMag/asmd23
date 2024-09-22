package u07.simulation.facade

import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.category.DefaultCategoryDataset

import java.awt.{Color, Dimension}
import javax.swing.JPanel

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
  def create(title: String = "", xAxisTitle: String = "", yAxisTitle: String = ""): LineChart2D =
    LineChart2D(title, xAxisTitle, yAxisTitle)

/**
 * Line chart as a [[JPanel]] using the [[org.jfree]] chart library.
 * @param title
 *   title of the chart
 * @param xAxisTitle
 *   label of the x-axis
 * @param yAxisTitle
 *   label of the y-axis
 */
class LineChart2D private (title: String, xAxisTitle: String, yAxisTitle: String):
  private val dataset = new DefaultCategoryDataset()
  private val chart = createChart
  private val panel = new ChartPanel(chart, false)
  panel.setFillZoomRectangle(true)
  panel.setMouseWheelEnabled(true)
  panel.setPreferredSize(new Dimension(500, 500))

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
  def addValue(x: Comparable[?], y: Double, row: Comparable[?]): Unit =
    dataset.addValue(y, row, x)

  /**
   * Creates a line chart.
   * @return
   *   A line chart.
   */
  private def createChart: JFreeChart =
    val chart = ChartFactory.createLineChart(title, xAxisTitle, yAxisTitle, dataset)
    chart.setBackgroundPaint(Color.WHITE)

    val plot = chart.getPlot.asInstanceOf[CategoryPlot]
    plot.setBackgroundPaint(Color.LIGHT_GRAY)
    plot.setDomainGridlinePaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.WHITE)
    plot.setAxisOffset(new RectangleInsets(1.0, 1.0, 1.0, 1.0))
    plot.setDomainCrosshairVisible(true)
    plot.setRangeCrosshairVisible(true)

    val r = plot.getRenderer
    r match
      case renderer: XYLineAndShapeRenderer =>
        renderer.setDefaultShapesVisible(true)
        renderer.setDefaultShapesFilled(true)
        renderer.setDrawSeriesLineAsPath(true)
      case _ =>

    chart
