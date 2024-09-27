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

  /**
   * Trait representing a general Controller for an event-driven simulation model.
   */
  trait Controller:
    /**
     * Abstract types for the view, model, model output, and events that drive the controller.
     */
    type View // Type representing the view (user interface).
    type Model // Type representing the model (system state).
    type ModelOut // Type representing the output of the model after an update.
    type Event // Type representing the events that can trigger updates.

    /**
     * The main loop for handling events, updating the model, and updating the view.
     *
     * @param events
     *   The lazy stream of events to process.
     * @param updateM
     *   The function to update the model based on the event.
     * @param updateV
     *   The function to update the view based on the updated model.
     * @param timeFactor
     *   The factor by which to scale time in the simulation.
     * @return
     *   A state transition function that updates both the model and the view.
     */
    def eventDrivenLoop(
      events: LazyList[Event],
      updateM: State[Model, ModelOut],
      updateV: ModelOut => State[View, Unit],
      timeFactor: Double
    ): State[(Model, View), Unit]

  /**
   * Concrete implementation of the Controller for a Stochastic Petri Net (SPN) simulation.
   *
   * @param model
   *   The SPN model instance that this controller will manage.
   * @tparam T
   *   The type of tokens or elements used in the SPN places.
   */
  class ControllerImpl[T](val model: SPNModel.SPNModelImpl[T]) extends Controller:
    // Specific types for this implementation
    override type View = Window // The view is a Window instance.
    override type Model = model.SPN // The model is an SPN instance.
    override type Event = String // Events are represented as strings.
    override type ModelOut = model.SystemUpdate // The model output is a SystemUpdate.

    import scala.concurrent.duration.DurationDouble

    // Minimum time delta for scheduled events (to avoid very small-time intervals)
    private val minDeltaTime: FiniteDuration = 20.millis

    /**
     * Implements the event-driven loop by processing events from a lazy stream. It updates both the model and the view
     * according to the current event.
     *
     * @param events
     *   A lazy stream of events to process.
     * @param updateM
     *   A state transition function for updating the model.
     * @param updateV
     *   A state transition function for updating the view based on the model's output.
     * @param timeFactor
     *   The factor to adjust time scaling in the simulation.
     * @return
     *   A state transition function that modifies the model and the view.
     */
    override def eventDrivenLoop(
      events: LazyList[this.Event],
      updateM: State[this.Model, this.ModelOut],
      updateV: this.ModelOut => State[this.View, Unit],
      timeFactor: Double
    ): State[(model.SPN, Window), Unit] =
      for
        // Process each event from the event stream
        _ <- seqN(events.map:
          case "Loop" =>
            // Update the model and view, and schedule the next iteration of the loop
            mv(
              updateM, // Update the model
              event =>
                seq(
                  updateV(event),
                  schedule(minDeltaTime.max((event.deltaTime * timeFactor).seconds))
                ) // Update the view and schedule the next loop
            )
          case _ => nop() // Handle other events as no-op
        )
      yield ()

    /**
     * Schedules the next event after a given period of time.
     *
     * @param period
     *   The delay period before the next event.
     * @return
     *   A state transition function that schedules the next event in the view.
     */
    private def schedule(period: FiniteDuration): State[this.View, Unit] =
      State(w => (w.schedule(period.toMillis.toInt, "Loop"), ()))

    /**
     * Represents a no-operation (no-op) state transition, used to indicate that no changes should be made to the model
     * or view.
     *
     * @return
     *   A state transition function that leaves both the model and the view unchanged.
     */
    def nop(): State[(model.SPN, Window), Unit] = mv(model.nop(), _ => SPNView.WindowStateImpl.nop())
