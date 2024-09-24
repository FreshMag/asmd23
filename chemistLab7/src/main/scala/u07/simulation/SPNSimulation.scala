package u07.simulation

object SPNSimulation:
  import u07.simulation.mvc.SPNView.WindowStateImpl.*
  import u07.simulation.mvc.SPNModel.SPNModelImplME.*
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

    private def printStepConsole(step: ModelOut): Unit =
      println(s"---> Delta Time: ${step.deltaTime}, Transition to: ${step.state})")

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

    private def createLoop(events: LazyList[Event]): State[(model.SPN, Window), Unit] =
      eventDrivenLoop(
        events,
        model.update(),
        event =>
          if debugPrints then printStepConsole(event)
          addChartValues(eventStateToChartValues(event.state.asMap, placesToHide), event.absoluteTime)
        ,
        timeFactor
      )

    private def controller =
      for
        events <- init
        _ <- createLoop(events)
      yield ()

    private val initialState = toCTMC(spn).newSimulationTrace(initialMarking, new Random)
    controller.run((initialState, initialWindow))
