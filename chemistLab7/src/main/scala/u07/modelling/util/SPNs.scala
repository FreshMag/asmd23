package u07.modelling.util

import u06.verifier.util.PetriNets.{Place3ME, PlaceRW, \}
import u06.verifier.util.PetriNets.Place3ME.*
import u06.verifier.util.PetriNets.PlaceRW.*
import u07.modelling.SPN
import u07.modelling.SPN.*

object SPNs:

  def mutualExclusion: SPN[Place3ME] = SPN[Place3ME](
    \(N) ~~ (_ => 1.0) ~~> \(T),
    \(T) ~~ (m => m(T)) ~~> \(C) ^^^ \(C),
    \(C) ~~ (_ => 2.0) ~~>  \()
  )

  def readersAndWriters: SPN[PlaceRW] = SPN[PlaceRW](
    \(P1) ~~ (_ => 1.0) ~~> \(P2),
    \(P2) ~~ (_ => 4.0) ~~> \(P3),
    \(P2) ~~ (_ => 1.0) ~~> \(P4),
    \(P3, Lock) ~~ (_ => 5.0) ~~> \(Lock, Reading),
    \(P4, Lock) ~~ (_ => 5.0) ~~> \(Writing) ^^^ \(Reading),
    \(Reading) ~~ (_ => 1.0) ~~> \(P1),
    \(Writing) ~~ (_ => 0.5) ~~> \(P1, Lock)
  )