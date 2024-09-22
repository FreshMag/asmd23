package u07.simulation

import u04.monads.States.State
import u06.verifier.util.PetriNets.Place3ME
import u06.verifier.util.PetriNets.Place3ME.*
import u07.modelling.SPN.*
import u07.simulation.SPNModel.SPNModelImplME.initialState
import u07.simulation.SPNView.WindowStateImpl.*

object ChartSimulationApp:

  def windowCreation(
    width: Int,
    height: Int,
    title: String,
    xLabel: String,
    yLabel: String,
    rowLabels: Iterable[String]
  ): State[Window, LazyList[String]] =
    for
      _ <- setSize(width, height)
      _ <- addChartView(title, xLabel, yLabel, rowLabels)
      _ <- show()
      events <- eventStream()
    yield events

object MutualExclusionController extends SPNController.ControllerImpl[Place3ME](SPNModel.SPNModelImplME) with App:

  private val controller =
    for
      events <- mv(
        model.nop(),
        _ =>
          ChartSimulationApp.windowCreation(
            1024,
            720,
            "Mutual exclusion",
            "Time",
            "Tokens",
            Place3ME.values.map(_.toString)
          )
      )
      _ <- gameLoop(
        events,
        model.update(),
        event =>
          addChartValues(
            event.state.asMap.map({ case (place, value) => (place.toString, value.toDouble) }),
            event.absoluteTime
          ),
        1.5
      )
    yield ()

  controller.run((initialState(\(N, N, N)), initialWindow))
