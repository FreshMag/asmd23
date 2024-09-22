package u07.simulation

import u06.verifier.util.PetriNets.Place3ME
import u07.examples.StochasticMutualExclusion
import u07.modelling.CTMCSimulation.Trace
import u07.modelling.SPN.{Marking, toCTMC}

import java.util.Random

object SPNModel:
  import u04.monads.States.State
  import u07.modelling.CTMCSimulation.Event
  import u07.modelling. CTMCSimulation. newSimulationTrace

  trait SPNModel[T]:
    type SPN
    type Time
    type System = (SPN, Time)
    type SystemUpdate
    def update(): State[System, SystemUpdate]
    def get(): State[System, System]
    def nop(): State[System, Unit]

  class SPNModelImpl[T] extends SPNModel[T]:
    
    override type SPN = Trace[Marking[T]]
    override type Time = Double
    
    case class SimulationStep(state: Marking[T], deltaTime: Double, absoluteTime: Double)
    override type SystemUpdate = SimulationStep
    
    override def update(): State[System, SimulationStep] =
      State((spn: SPN, time: Time) =>
        val nextEvent = spn.head
        val newTrace = spn.drop(1)
        println(Event(nextEvent.time - time, nextEvent.state))
        ((newTrace, nextEvent.time), SimulationStep(nextEvent.state, nextEvent.time - time, nextEvent.time))
      )

    override def nop(): State[System, Unit] = State(system => (system, ()))

    override def get(): State[(Trace[Marking[T]], Double), (Trace[Marking[T]], Double)] =
      State(s => (s, s))

  object SPNModelImplME extends SPNModelImpl[Place3ME]:
    def initialState(initialMarking: Marking[Place3ME]): System =
      val spn = toCTMC(StochasticMutualExclusion.spn)
      (spn.newSimulationTrace(initialMarking, new Random), 0.0)
