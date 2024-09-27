package u07.simulation

object SPNVisualization:
  import u07.modelling.CTMCSimulation.newSimulationTrace
  import u07.modelling.SPN.{Marking, SPN, toCTMC}
  import u07.simulation.mvc.SPNView.WindowStateImpl.*
  import org.jfree.data.xy.XYSeriesCollection
  import u06.utils.MSet
  import u07.simulation.facade.SwingFunctionalFacade
  import u07.simulation.facade.SwingFunctionalFacade.Frame
  import u07.simulation.utils.SPNUtils

  import java.util.Random

  /**
   * The SPNVisualization class provides a graphical interface for visualizing the simulation of a Stochastic Petri Net
   * (SPN).
   *
   * @param spn
   *   The Stochastic Petri Net being visualized.
   * @param places
   *   The places in the SPN.
   * @param initialMarking
   *   The initial token distribution in the SPN.
   * @param placesToHide
   *   A set of places to hide from the visualization.
   * @param simulationLength
   *   The number of simulation steps to display.
   * @param title
   *   The title of the visualization window.
   * @param xLabel
   *   The label for the x-axis of the chart.
   * @param yLabel
   *   The label for the y-axis of the chart.
   * @param windowWidth
   *   The width of the visualization window.
   * @param windowHeight
   *   The height of the visualization window.
   */
  class SPNVisualization[T](
    spn: SPN[T],
    places: Iterable[T],
    initialMarking: Marking[T],
    placesToHide: Set[T] = Set[T](),
    simulationLength: Int = 100,
    title: String = "Simulation",
    xLabel: String = "Time",
    yLabel: String = "Tokens",
    windowWidth: Int = 1024,
    windowHeight: Int = 720
  ) extends App:

    /**
     * Initializes the graphical frame for the SPN visualization.
     *
     * @return
     *   The created frame for displaying the chart.
     */
    private def initFrame: Frame = SwingFunctionalFacade.createFrame()
      .setSize(windowWidth, windowHeight)
      .createChartWithDataset(title, xLabel, yLabel, placesToTrack.map(_.toString), dataset)

    /**
     * Simulation trace from the SPN model and takes a specified number of events.
     */
    private val trace = toCTMC(spn).newSimulationTrace(initialMarking, new Random).take(simulationLength)

    /**
     * Determines the places to track by excluding the places to hide from the provided places.
     */
    private val placesToTrack = places.toSet.diff(placesToHide)

    /**
     * Initializes the dataset for the chart.
     */
    private val dataset = new XYSeriesCollection()

    /**
     * Initializes the frame for the SPN visualization.
     */
    private val frame = initFrame

    /*
     * Populates the chart with data from the simulation trace.
     * Iterates through each event in the trace, adding the relevant data to the chart.
     */
    trace.foreach: event =>
      frame.addChartValues(SPNUtils.eventStateToChartValues(event.state.asMap, placesToHide), event.time)

    /*
     * Displays the frame containing the simulation chart.
     */
    frame.show()
