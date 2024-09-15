package u06.verifier

export u06.modelling.PetriNet
import u06.utils.MSet
import u06.modelling.System

object ReadersAndWriters:

  enum Place:
    case P1, P2, P3, P4, P5, P6, P7

  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  // DSL-like specification of a Petri Net
  def pnME: System[Marking[Place]] = PetriNet[Place](
    MSet(P1) ~~> MSet(P2),
    MSet(P2) ~~> MSet(P3),
    MSet(P2) ~~> MSet(P4),
    MSet(P3, P5) ~~> MSet(P5, P6),
    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6),
    MSet(P6) ~~> MSet(P1),
    MSet(P7) ~~> MSet(P1, P5)
  ).toSystem

@main def runReadersAndWriters(): Unit =
  import ReadersAndWriters.*
  // example usage
  println(
    ((1 to 60).to(LazyList) flatMap (pnME.paths(MSet(P1, P5), _)))
      .flatMap(_.toList)
      .map(marking => marking.asMap)
      .exists(tokensIn => tokensIn.getOrElse(P6, 0) > 0 && tokensIn.getOrElse(P7, 0) > 0)
  )
