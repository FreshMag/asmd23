package u07.simulation.mvc

object SPNController:
  import u04.monads.Monads.*
  import u04.monads.Monads.Monad.*
  import u04.monads.States.State
  import u04.monads.States.State.*
  import u07.simulation.mvc.SPNView.WindowStateImpl.Window
  import u07.simulation.utils.SPNUtils.*

  import scala.collection.immutable.LazyList
  import scala.concurrent.duration.FiniteDuration

  trait Controller:
    type View
    type Model
    type ModelOut
    type Event

    def eventDrivenLoop(
      events: LazyList[Event],
      updateM: State[Model, ModelOut],
      updateV: ModelOut => State[View, Unit],
      timeFactor: Double
    ): State[(Model, View), Unit]

  class ControllerImpl[T](val model: SPNModel.SPNModelImpl[T]) extends Controller:
    override type View = Window
    override type Model = model.SPN
    override type Event = String
    override type ModelOut = model.SystemUpdate

    import scala.concurrent.duration.DurationDouble

    private val minDeltaTime: FiniteDuration = 20.millis

    override def eventDrivenLoop(
      events: LazyList[this.Event],
      updateM: State[this.Model, this.ModelOut],
      updateV: this.ModelOut => State[this.View, Unit],
      timeFactor: Double
    ): State[(model.SPN, Window), Unit] =
      for
        _ <- seqN(events.map:
          case "Loop" =>
            mv(
              updateM,
              event => seq(updateV(event), schedule(minDeltaTime.max((event.deltaTime * timeFactor).seconds)))
            )
          case _ => nop()
        )
      yield ()

    private def schedule(period: FiniteDuration): State[this.View, Unit] =
      State(w => (w.schedule(period.toMillis.toInt, "Loop"), ()))

    def nop(): State[(model.SPN, Window), Unit] = mv(model.nop(), _ => SPNView.WindowStateImpl.nop())
