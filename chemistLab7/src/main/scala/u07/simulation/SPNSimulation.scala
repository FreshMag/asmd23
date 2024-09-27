package u07.simulation

/**
 * The SPNSimulation object contains the implementation of a simulation for a Stochastic Petri Net (SPN). It follows the
 * Model-View-Controller (MVC) architecture and uses a graphical interface for visualization.
 */
object SPNSimulation:
  import u07.simulation.mvc.SPNView.WindowStateImpl.*
  import u07.simulation.mvc.SPNController.*
  import u07.simulation.mvc.SPNModel.SPNModelImpl
  import u04.monads.States.State
  import u07.modelling.SPN.Marking
  import u07.modelling.SPN.SPN
  import u07.modelling.CTMCSimulation.newSimulationTrace
  import u07.modelling.SPN.toCTMC
  import u07.simulation.utils.SPNUtils.*

  import java.util.Random

  private object ChartSimulationApp:

    /**
     * Creates and initializes a window for the SPN simulation.
     *
     * @param width
     *   The width of the window.
     * @param height
     *   The height of the window.
     * @param title
     *   The title of the chart.
     * @param xLabel
     *   The label for the x-axis.
     * @param yLabel
     *   The label for the y-axis.
     * @param rowLabels
     *   Labels for each row in the chart.
     * @param chartInitialSetup
     *   Initial setup values for the chart.
     * @return
     *   A state that provides a stream of events from the window.
     */
    def windowCreation(
      width: Int,
      height: Int,
      title: String,
      xLabel: String,
      yLabel: String,
      rowLabels: Iterable[String],
      chartInitialSetup: Map[String, Double]
    ): State[Window, LazyList[String]] =
      for
        _ <- setSize(width, height)
        _ <- addButton("Start", "Loop")
        _ <- addChartView(title, xLabel, yLabel, rowLabels)
        _ <- addChartValues(chartInitialSetup, 0)
        _ <- show()
        events <- eventStream()
      yield events

  /**
   * The SPNSimulation class handles the overall simulation of the SPN. It extends the ControllerImpl class, defining
   * the model, view, and event loop.
   *
   * @param spn
   *   The Stochastic Petri Net being simulated.
   * @param places
   *   The places in the SPN.
   * @param initialMarking
   *   The initial token distribution in the SPN.
   * @param placesToHide
   *   Optional places to hide in the visualization.
   * @param title
   *   Title for the simulation window.
   * @param xLabel
   *   Label for the x-axis of the chart.
   * @param yLabel
   *   Label for the y-axis of the chart.
   * @param windowWidth
   *   Width of the simulation window.
   * @param windowHeight
   *   Height of the simulation window.
   * @param timeFactor
   *   Factor to adjust the speed of the simulation.
   * @param debugPrints
   *   Flag to enable or disable debug prints.
   */
  class SPNSimulation[T](
    spn: SPN[T],
    places: Iterable[T],
    initialMarking: Marking[T],
    placesToHide: Set[T] = Set[T](),
    title: String = "Simulation",
    xLabel: String = "Time",
    yLabel: String = "Tokens",
    windowWidth: Int = 1024,
    windowHeight: Int = 720,
    timeFactor: Double = 1.0,
    debugPrints: Boolean = false
  ) extends ControllerImpl[T](new SPNModelImpl[T]) with App:

    /**
     * Prints the details of the current simulation step to the console.
     *
     * @param step
     *   The current step in the simulation, containing delta time and state.
     */
    private def printStepConsole(step: ModelOut): Unit =
      println(s"---> Delta Time: ${step.deltaTime}, Transition to: ${step.state})")

    /**
     * Initializes the simulation by creating the window and setting up the initial state.
     *
     * @return
     *   A state that provides the stream of events from the window.
     */
    private def init: State[(model.SPN, Window), LazyList[Event]] =
      mv(
        model.get(),
        spnTrace =>
          ChartSimulationApp.windowCreation(
            windowWidth,
            windowHeight,
            title,
            xLabel,
            yLabel,
            places.toSet.diff(placesToHide).map(_.toString),
            eventStateToChartValues(spnTrace.head.state.asMap, placesToHide)
          )
      )

    /**
     * Creates the event-driven loop for the simulation.
     *
     * @param events
     *   The stream of events generated from the window.
     * @return
     *   A state that runs the event loop and updates the model and view.
     */
    private def createLoop(events: LazyList[Event]): State[(model.SPN, Window), Unit] =
      eventDrivenLoop(
        events,
        model.update(),
        updateStep =>
          if debugPrints then printStepConsole(updateStep)
          addChartValues(eventStateToChartValues(updateStep.state.asMap, placesToHide), updateStep.absoluteTime)
        ,
        timeFactor
      )

    /**
     * The controller function that coordinates the initialization and looping of the simulation.
     *
     * @return
     *   A state that runs the controller logic.
     */
    private def controller =
      for
        events <- init
        _ <- createLoop(events)
      yield ()

    /**
     * The initial state for the simulation, creating a trace from the SPN model.
     */
    private val initialState = toCTMC(spn).newSimulationTrace(initialMarking, new Random)

    /**
     * Runs the controller with the initial state and window.
     */
    controller.run((initialState, initialWindow))
