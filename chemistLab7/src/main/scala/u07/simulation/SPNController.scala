package u07.simulation

import u04.monads.Monads.*
import u04.monads.Monads.Monad.*
import u04.monads.States.State
import u04.monads.States.State.*
import u07.simulation.SPNView.WindowStateImpl.Window

import scala.collection.immutable.LazyList
import scala.concurrent.duration.FiniteDuration

object SPNController:

  trait Controller:
    type View
    type Model
    type ModelOut
    type Event

    def gameLoop(
      events: LazyList[Event],
      updateM: State[Model, ModelOut],
      updateV: ModelOut => State[View, Unit],
      timeFactor: Double
    ): State[(Model, View), Unit]

  class ControllerImpl[T](val model: SPNModel.SPNModelImpl[T]) extends Controller:
    override type View = Window
    override type Model = model.System
    override type Event = String
    override type ModelOut = model.SystemUpdate

    import scala.concurrent.duration.DurationDouble

    private val minDeltaTime: FiniteDuration = 20.millis

    override def gameLoop(events: LazyList[this.Event], 
                          updateM: State[this.Model, this.ModelOut], 
                          updateV: this.ModelOut => State[this.View, Unit],
                          timeFactor: Double
    ): State[(model.System, Window), Unit] =
      for
        _ <- mv(updateM, event => loop(event.deltaTime.seconds))
        _ <- seqN(events.map:
          case "Loop" => mv(updateM, event => seq(updateV(event),
            loop(minDeltaTime.max((event.deltaTime * timeFactor).seconds))))
          case _ => nop()
        )
      yield ()

    def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
      State:
        case (sm, sv) =>
          val (sm2, am) = m1.run(sm)
          val (sv2, av) = f(am).run(sv)
          ((sm2, sv2), av)

    private def loop(period: FiniteDuration): State[this.View, Unit] =
      State(w => (w.schedule(period.toMillis.toInt, "Loop"), ()))
      
    def nop(): State[(model.System, Window), Unit] = mv(model.nop(), _ => SPNView.WindowStateImpl.nop())
