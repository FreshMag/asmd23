package u07.simulation

import u04.monads.States.State
import u06.verifier.util.PetriNets.Place3ME
import u06.verifier.util.PetriNets.Place3ME.*
import u07.modelling.SPN.*
import u07.simulation.SPNModel.SPNModelImplME.initialState
import u07.simulation.SPNView.WindowStateImpl.*

object ChartSimulationApp:

  def windowCreation(width: Int, height: Int): State[Window, LazyList[String]] =
    for
      _ <- setSize(width, height)
      _ <- addChartView()
      _ <- show()
      events <- eventStream()
    yield events

object MutualExclusionController extends SPNController.ControllerImpl[Place3ME](SPNModel.SPNModelImplME) with App:

  val controller =
    for
      events <- mv(model.nop(), _ => ChartSimulationApp.windowCreation(500, 500))
      _ <- gameLoop(events, model.update(), _ => nop(), 1.5)
    yield ()

  controller.run((initialState(\(N, N, N)), initialWindow))
