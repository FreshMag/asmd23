package u07.examples

import u07.modelling.{CTMC, SPN}
import u06.utils.MSet

import java.util.Random

object StochasticMutualExclusion extends App:
  // Specification of my data-type for states
  import u06.verifier.util.PetriNets.Place3ME.*
  import u06.verifier.util.PetriNets.Place3ME
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*
  
  def spn = SPN[Place3ME](
    \(N) ~~ (_ => 1.0) ~~> \(T),
    \(T) ~~ (m => m(T)) ~~> \(C) ^^^ \(C),
    \(C) ~~ (_ => 2.0) ~~>  \()
  )

//  println:
//    toCTMC(spn).newSimulationTrace(\(N,N,N,N), new Random)
//      .take(20)
//      .toList.mkString("\n")

