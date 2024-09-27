package u07.simulation

import u06.verifier.util.PetriNets.{Place3ME, PlaceBrusselator, PlaceRW, \}
import u06.verifier.util.PetriNets.Place3ME.*
import u06.verifier.util.PetriNets.PlaceRW.*
import u06.verifier.util.PetriNets.PlaceBrusselator.*
import u07.simulation.SPNSimulation.SPNSimulation
import u07.modelling.util.SPNs.*
import u07.simulation.SPNVisualization.SPNVisualization

object MutualExclusionSimulation extends SPNSimulation(
      spn = mutualExclusion,
      places = Place3ME.values,
      initialMarking = \(N, N, N),
      title = "Mutual Exclusion"
    )

object MutualExclusionVisualization extends SPNVisualization(
  spn = mutualExclusion,
  places = Place3ME.values,
  initialMarking = \(N, N, N, N),
  title = "Mutual Exclusion",
  simulationLength = 100
)

object ReadersAndWritersSimulation extends SPNSimulation(
      spn = readersAndWriters,
      places = PlaceRW.values,
      initialMarking = \(P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, Lock),
      placesToHide = Set(P1, P2, P3, P4, Lock),
      title = "Readers and Writers",
      timeFactor = 0.5,
      debugPrints = true
    )

object ReadersAndWritersVisualization extends SPNVisualization(
  spn = readersAndWriters,
  places = PlaceRW.values,
  initialMarking = \(P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, P1, Lock),
  placesToHide = Set(P1, P2, P3, P4, Lock),
  title = "Readers and Writers",
  simulationLength = 100
)

object BrusselatorSimulation extends SPNSimulation(
      spn = brusselator,
      places = PlaceBrusselator.values,
      initialMarking = 1 * \(A) + 1 * \(B),
      title = "Brusselator",
      placesToHide = Set(A, B, D, E),
      timeFactor = 0.1,
      debugPrints = true
    )

object BrusselatorVisualization extends SPNVisualization(
  spn = brusselator,
  places = PlaceBrusselator.values,
  initialMarking = 1 * \(A) + 1 * \(B),
  title = "Brusselator",
  placesToHide = Set(A, B, D, E),
  simulationLength = 10000
)