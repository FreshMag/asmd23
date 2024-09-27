package u07.simulation.mvc

import u06.verifier.util.PetriNets.Place3ME
import u07.modelling.CTMCSimulation.Trace
import u07.modelling.SPN.{Marking, toCTMC}

import java.util.Random

object SPNModel:
  import u04.monads.States.State
  import u07.modelling.CTMCSimulation.newSimulationTrace

  /**
   * Trait representing the Stochastic Petri Net (SPN) model for simulation.
   *
   * @tparam T
   *   The type representing the tokens or elements in the SPN places.
   */
  trait SPNModel[T]:
    /**
     * Abstract type for the SPN (trace of events).
     */
    type SPN // Type representing the trace of SPN events.
    type Time // Type representing the time in the simulation.
    type SystemUpdate // Type representing the update information of the system.

    /**
     * Update the SPN model by progressing to the next state in the simulation trace.
     *
     * @return
     *   A state transition function that returns a system update (i.e., the next event).
     */
    def update(): State[SPN, SystemUpdate]

    /**
     * Retrieve the current state of the SPN model.
     *
     * @return
     *   A state transition function that returns the current state of the SPN.
     */
    def get(): State[SPN, SPN]

    /**
     * Represents a no-operation (nop) in the model. Used when no updates are made.
     *
     * @return
     *   A state transition function that leaves the SPN unchanged.
     */
    def nop(): State[SPN, Unit]

  /**
   * Concrete implementation of the SPNModel for running a simulation on a given SPN trace.
   *
   * @tparam T
   *   The type representing the tokens or elements in the SPN places.
   */
  class SPNModelImpl[T] extends SPNModel[T]:

    // The SPN is represented as a trace of events over time.
    override type SPN = Trace[Marking[T]] // A trace of markings (states) in the SPN.
    override type Time = Double // Time is represented as a Double.

    /**
     * Case class representing a step in the simulation, containing the state, delta time, and absolute time.
     *
     * @param state
     *   The marking (state) of the SPN at this step.
     * @param deltaTime
     *   The time difference between the current and the next event.
     * @param absoluteTime
     *   The absolute time of the current event.
     */
    case class SimulationStep(state: Marking[T], deltaTime: Double, absoluteTime: Double)

    // System update is a single simulation step.
    override type SystemUpdate = SimulationStep

    /**
     * Advances the SPN simulation to the next event and returns the state at that step.
     *
     * @return
     *   A state transition function that moves to the next event in the trace and returns a SimulationStep.
     */
    override def update(): State[SPN, SimulationStep] =
      State((spn: SPN) =>
        val currentEvent = spn.head // Get the current event.
        val newTrace = spn.drop(1) // Advance to the next event.
        val nextEvent = newTrace.head // Get the next event in the trace.
        (
          newTrace,
          // Return a new SimulationStep with the state, delta time, and absolute time.
          SimulationStep(currentEvent.state, (nextEvent.time - currentEvent.time).max(0), currentEvent.time)
        )
      )

    /**
     * Represents a no-operation (nop) in the SPN simulation. This is used when no transition is applied, leaving the
     * system unchanged.
     *
     * @return
     *   A state transition function that returns the current state unchanged.
     */
    override def nop(): State[SPN, Unit] = State(system => (system, ()))

    /**
     * Retrieves the current state of the SPN simulation.
     *
     * @return
     *   A state transition function that returns the current trace of events (SPN).
     */
    override def get(): State[SPN, SPN] =
      State(s => (s, s))

  /**
   * An object representing a specific SPN model for mutual exclusion (ME) using Place3ME as the place type.
   */
  object SPNModelImplME extends SPNModelImpl[Place3ME]:
    import u07.modelling.util.SPNs.mutualExclusion

    /**
     * Initializes the SPN simulation with the given initial marking for the mutual exclusion model.
     *
     * @param initialMarking
     *   The initial marking (state) of the SPN.
     * @return
     *   The initial trace of the simulation (SPN) for mutual exclusion.
     */
    def initialState(initialMarking: Marking[Place3ME]): SPN =
      val spn = toCTMC(mutualExclusion) // Convert the mutual exclusion SPN to a CTMC.
      spn.newSimulationTrace(initialMarking, new Random) // Generate the initial trace with random transitions.
