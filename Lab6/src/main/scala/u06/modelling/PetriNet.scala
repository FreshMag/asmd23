/**
 * Modified version of the original `PetriNet.scala` from Lab6
 */
package u06.modelling

import u06.utils.MSet

import scala.annotation.targetName

object PetriNet:
  // pre-conditions, effects, inhibition
  case class Trn[P](cond: MSet[P], eff: MSet[P], inh: MSet[P])
  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  // factory of A Petri Net
  def apply[P](transitions: Trn[P]*): PetriNet[P] = transitions.toSet

  // factory of a System, as a toSystem method
  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      for
        Trn(cond, eff, inh) <- pn // get any transition
        if m disjointed inh // check inhibition
        out <- m extract cond // remove precondition
      yield out union eff // add effect

  // fancy syntax to create transition rules
  extension [P](self: Marking[P])
    @targetName("transition")
    infix def ~~>(y: Marking[P]): Trn[P] = Trn(self, y, MSet())
  extension [P](self: Trn[P])
    @targetName("inhibitedBy")
    infix def ^^^(z: Marking[P]): Trn[P] = self.copy(inh = z)
