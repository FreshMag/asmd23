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
    type SystemEvent
    def update(): State[System, SystemEvent]
    def nop(): State[System, Unit]

  class SPNModelImpl[T] extends SPNModel[T]:
    override type SPN = Trace[Marking[T]]
    override type Time = Double
    override type SystemEvent = Event[Marking[T]]
    
    override def update(): State[System, Event[Marking[T]]] =
      State((spn: SPN, time: Time) =>
        val nextEvent = spn.head
        val newTrace = spn.drop(1)
        println(Event(nextEvent.time - time, nextEvent.state))
        ((newTrace, nextEvent.time), Event(nextEvent.time - time, nextEvent.state))
      )

    override def nop(): State[System, Unit] = State(system => (system, ()))

  object SPNModelImplME extends SPNModelImpl[Place3ME]:
    def initialState(initialMarking: Marking[Place3ME]): System =
      val spn = toCTMC(StochasticMutualExclusion.spn)
      (spn.newSimulationTrace(initialMarking, new Random), 0.0)
