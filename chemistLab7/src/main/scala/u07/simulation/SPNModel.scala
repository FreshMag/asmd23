package u07.simulation

import u06.verifier.util.PetriNets.Place3ME
import u07.examples.StochasticMutualExclusion
import u07.modelling.CTMCSimulation.Trace
import u07.modelling.SPN.{Marking, toCTMC}

import java.util.Random

object SPNModel:
  import u04.monads.States.State
  import u07.modelling.CTMCSimulation.newSimulationTrace

  trait SPNModel[T]:
    type SPN
    type Time
    type SystemUpdate
    def update(): State[SPN, SystemUpdate]
    def get(): State[SPN, SPN]
    def nop(): State[SPN, Unit]

  class SPNModelImpl[T] extends SPNModel[T]:

    override type SPN = Trace[Marking[T]]
    override type Time = Double

    case class SimulationStep(state: Marking[T], deltaTime: Double, absoluteTime: Double)
    override type SystemUpdate = SimulationStep

    override def update(): State[SPN, SimulationStep] =
      State((spn: SPN) =>
        val currentEvent = spn.head
        val newTrace = spn.drop(1)
        val nextEvent = newTrace.head
        println(s"Event(${currentEvent.time}, ${currentEvent.state}) ---> Event(${nextEvent.time}, ${nextEvent.state})")
        (
          newTrace,
          SimulationStep(currentEvent.state, (nextEvent.time - currentEvent.time).max(0), currentEvent.time)
        )
      )

    override def nop(): State[SPN, Unit] = State(system => (system, ()))

    override def get(): State[SPN, SPN] =
      State(s => (s, s))

  object SPNModelImplME extends SPNModelImpl[Place3ME]:
    def initialState(initialMarking: Marking[Place3ME]): SPN =
      val spn = toCTMC(StochasticMutualExclusion.spn)
      spn.newSimulationTrace(initialMarking, new Random)
