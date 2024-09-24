package u07.simulation

import u07.modelling.SPN.toCTMC

import java.util.Random

object SPNSimulation:
  import u07.simulation.mvc.SPNView.WindowStateImpl.*
  import u07.simulation.mvc.SPNModel.SPNModelImplME.*
  import u07.simulation.mvc.SPNController.*
  import u07.simulation.mvc.SPNModel.SPNModelImpl
  import u04.monads.States.State
  import u07.modelling.SPN.Marking
  import u07.modelling.SPN.SPN
  import u07.modelling.CTMCSimulation.newSimulationTrace

  object ChartSimulationApp:

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
    timeFactor: Double = 1.0
  ) extends ControllerImpl[T](new SPNModelImpl[T]) with App:

    private val controller =
      for
        events <- mv(
          model.get(),
          spn =>
            ChartSimulationApp.windowCreation(
              windowWidth,
              windowHeight,
              title,
              xLabel,
              yLabel,
              places.toSet.diff(placesToHide).map(_.toString),
              spn.head.state.asMap.map:
                case (place, count) => (place.toString, count.toDouble)
            )
        )
        _ <- gameLoop(
          events,
          model.update(),
          event =>
            addChartValues(
              event.state.asMap
                .filter((place, _) => !placesToHide.contains(place))
                .map({ case (place, value) => (place.toString, value.toDouble) }),
              event.absoluteTime
            ),
          timeFactor
        )
      yield ()

    private val initialState = toCTMC(spn).newSimulationTrace(initialMarking, new Random)
    controller.run((initialState, initialWindow))
