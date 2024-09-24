package u07.simulation


import u06.verifier.util.PetriNets.{Place3ME, PlaceRW, \}
import u06.verifier.util.PetriNets.Place3ME.*
import u06.verifier.util.PetriNets.PlaceRW.*
import u07.simulation.SPNSimulation.SPNSimulation
import u07.modelling.util.SPNs.*

object MutualExclusionSimulation extends SPNSimulation(
  spn = mutualExclusion,
  places = Place3ME.values,
  initialMarking = \(N, N, N),
  title = "Mutual Exclusion",
)

object ReadersAndWriters extends SPNSimulation(
  spn = readersAndWriters,
  places = PlaceRW.values,
  initialMarking = \(P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, Lock),
  placesToHide = Set(P1, P2, P3, P4, Lock),
  title = "Readers and Writers",
  timeFactor = 0.5
)
