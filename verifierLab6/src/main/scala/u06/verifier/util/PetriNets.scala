package u06.verifier.util

object PetriNets:

  export u06.modelling.PetriNet.*
  export u06.utils.MSet
  export u06.modelling.PetriNet
  export u06.modelling.SystemAnalysis.*
  export u06.modelling.System

  enum Place2:
    case P1, P2

  enum Place3:
    case P1, P2, P3

  enum Place3ME:
    case N, T, C

  enum Place4:
    case P1, P2, P3, P4

  enum Place5:
    case P1, P2, P3, P4, P5

  enum Place6:
    case P1, P2, P3, P4, P5, P6

  enum Place7:
    case P1, P2, P3, P4, P5, P6, P7

  enum PlaceRW:
    case P1, P2, P3, P4, Lock, Reading, Writing

  enum PlaceBrusselator:
    case A, B, X, Y, D, E

  def \[A](l: A*): MSet[A] = MSet(l*)

  def PN[T](transitions: Trn[T]*): System[Marking[T]] = PetriNet[T](transitions*).toSystem

  import Place7.*
  def readersAndWriters: System[Marking[Place7]] = PN[Place7](
    \(P1) ~~> \(P2),
    \(P2) ~~> \(P3),
    \(P2) ~~> \(P4),
    \(P3, P5) ~~> \(P5, P6),
    \(P4, P5) ~~> \(P7) ^^^ \(P6),
    \(P6) ~~> \(P1),
    \(P7) ~~> \(P1, P5)
  )

  import Place3ME.*
  def mutualExclusion: System[Marking[Place3ME]] = PN[Place3ME](
    \(N) ~~> \(T),
    \(T) ~~> \(C) ^^^ \(C),
    \(C) ~~> \()
  )
