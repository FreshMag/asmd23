package u07.chemist

import u07.modelling.CTMC.*
import u07.modelling.{CTMC, SPN}
import u06.utils.MSet

import java.util.Random

object Chemist extends App:
  // Specification of my data-type for states
  enum Place:
    case A,B,X,Y,D,E

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val spn = SPN[Place](
    Trn(MSet(A), m => 1.0,   MSet(X),  MSet()),
    Trn(MSet(X,X,Y), m => 1.0,  MSet(X,X,X),  MSet()),
    Trn(MSet(B,X), m => 1.0,   MSet(Y,D),   MSet()),
    Trn(MSet(X), m => 1.0, MSet(E), MSet()))

  private val starting =  LazyList.continually(List(A,A,A,X,Y,B)).flatten.take(100).toList
  println:
    toCTMC(spn).newSimulationTrace(MSet(starting*),new Random)
      .take(100)
      .toList.mkString("\n")