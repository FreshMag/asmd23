package u07.simulation.facade

import org.jfree.data.xy.XYSeriesCollection
import java.util.concurrent.LinkedBlockingQueue
import javax.swing.*
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import scala.util.Try

object SwingFunctionalFacade:

  /**
   * Creates and returns a new instance of a frame for building the GUI.
   *
   * @return
   *   A new Frame object for setting up and displaying the user interface.
   */
  def createFrame(): Frame = new FrameImpl()

  /**
   * Trait representing a functional interface for building a GUI frame with charts and buttons.
   */
  trait Frame:
    /**
     * Sets the size of the frame.
     *
     * @param width
     *   The width of the frame.
     * @param height
     *   The height of the frame.
     * @return
     *   The current frame instance for chaining.
     */
    def setSize(width: Int, height: Int): Frame

    /**
     * Creates a chart inside the frame with specified labels.
     *
     * @param title
     *   The title of the chart.
     * @param xLabel
     *   The label for the X-axis.
     * @param yLabel
     *   The label for the Y-axis.
     * @param rowLabels
     *   The row labels for chart data series.
     * @return
     *   The current frame instance for chaining.
     */
    def createChart(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): Frame

    /**
     * Creates a chart with a custom dataset inside the frame.
     *
     * @param title
     *   The title of the chart.
     * @param xLabel
     *   The label for the X-axis.
     * @param yLabel
     *   The label for the Y-axis.
     * @param rowLabels
     *   The row labels for chart data series.
     * @param dataset
     *   The custom dataset to use for the chart.
     * @return
     *   The current frame instance for chaining.
     */
    def createChartWithDataset(
      title: String,
      xLabel: String,
      yLabel: String,
      rowLabels: Iterable[String],
      dataset: XYSeriesCollection
    ): Frame

    /**
     * Adds a button to the frame.
     *
     * @param text
     *   The text to display on the button.
     * @param name
     *   The action command name for the button (used for event handling).
     * @return
     *   The current frame instance for chaining.
     */
    def addButton(text: String, name: String): Frame

    /**
     * Adds a single (x, y) value to the chart with a specified row key (data series).
     *
     * @param x
     *   The X value.
     * @param y
     *   The Y value.
     * @param rowKey
     *   The key for the data series in the chart.
     * @return
     *   The current frame instance for chaining.
     */
    def addChartValue(x: Double, y: Double, rowKey: String): Frame

    /**
     * Adds multiple values (with row keys) to the chart for a given X value.
     *
     * @param values
     *   A map of row keys to Y values.
     * @param x
     *   The X value.
     * @return
     *   The current frame instance for chaining.
     */
    def addChartValues(values: Map[String, Double], x: Double): Frame

    /**
     * Schedules an event to occur after a certain delay.
     *
     * @param millis
     *   The delay in milliseconds before the event occurs.
     * @param eventName
     *   The name of the event to trigger.
     * @return
     *   The current frame instance for chaining.
     */
    def schedule(millis: Int, eventName: String): Frame

    /**
     * Makes the frame visible to the user.
     *
     * @return
     *   The current frame instance for chaining.
     */
    def show(): Frame

    /**
     * Retrieves the event that occurred (e.g., button click or scheduled event).
     *
     * @return
     *   A function that provides the next event as a String.
     */
    def events(): () => String

  /**
   * Implementation of the Frame trait using Swing components.
   */
  class FrameImpl extends Frame:
    private val jFrame: JFrame = new JFrame()
    private val content: JPanel = new JPanel()
    content.setLayout(null) // Setting layout to null, to manually manage component positions
    private var jChart: LineChart2D = LineChart2D.create() // Initialize an empty chart
    private var chartRowLabels: Iterable[String] = Seq() // Store the labels for the chart rows
    private val eventQueue: LinkedBlockingQueue[String] = new LinkedBlockingQueue() // Event queue to handle events

    // Function that provides the next event, or an empty string if none exists
    override def events(): () => String = () =>
      Try(eventQueue.take()).getOrElse("")

    // Set up content layout for the frame
    this.content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS))
    this.jFrame.add(content)
    this.jFrame.setDefaultCloseOperation(EXIT_ON_CLOSE)

    // Set the size of the frame
    override def setSize(width: Int, height: Int): Frame =
      this.jFrame.setSize(width, height)
      this

    // Create a chart inside the frame
    override def createChart(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): Frame =
      createChartWithDataset(title, xLabel, yLabel, rowLabels, new XYSeriesCollection())

    // Create a chart with a custom dataset inside the frame
    override def createChartWithDataset(
      title: String,
      xLabel: String,
      yLabel: String,
      rowLabels: Iterable[String],
      dataset: XYSeriesCollection
    ): Frame =
      jChart = LineChart2D.create(title, xLabel, yLabel, dataset) // Create the chart
      content.add(jChart.asJPanel) // Add the chart to the frame
      chartRowLabels = rowLabels // Store the row labels for later use
      this

    // Add a button to the frame
    override def addButton(text: String, name: String): Frame =
      val jb: JButton = new JButton(text) // Create a new button
      jb.setActionCommand(name) // Set the action command (event name) for the button
      jb.addActionListener(_ => Try(eventQueue.put(name))) // Add the event to the queue when clicked
      this.content.add(jb) // Add the button to the frame
      this

    // Add a single (x, y) value to the chart for a given row key
    override def addChartValue(x: Double, y: Double, rowKey: String): Frame =
      jChart.addValue(x, y, rowKey) // Add the value to the chart
      this

    // Add multiple values for a given X value to the chart
    override def addChartValues(values: Map[String, Double], x: Double): Frame =
      values.foreach { (rowId, y) =>
        jChart.addValue(x, y, rowId) // Add each value to the chart
      }
      // For any row labels not in the map, add a value of 0 at the current X
      chartRowLabels.filter(!values.contains(_)).foreach {
        jChart.addValue(x, 0, _)
      }
      this

    // Schedule an event to occur after a given delay (in milliseconds)
    override def schedule(millis: Int, eventName: String): Frame =
      val timer = new Timer(millis, _ => Try(eventQueue.put(eventName))) // Set up a timer to trigger the event
      timer.setRepeats(false) // The timer will not repeat
      timer.start() // Start the timer
      this

    // Show the frame, making it visible
    override def show(): Frame =
      jFrame.setVisible(true) // Make the frame visible
      this
