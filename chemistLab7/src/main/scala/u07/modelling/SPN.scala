package u07.modelling

import u06.utils.MSet
import u07.modelling.CTMC.*
import scala.annotation.targetName

object SPN:

  // pre-conditions, rate, effects, inhibition
  case class Trn[P](
    cond: Marking[P],
    rate: Marking[P] => Double,
    eff: Marking[P],
    inh: Marking[P]
  )

  type SPN[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  export u06.verifier.util.PetriNets.\

  def toCTMC[P](spn: SPN[P]): CTMC[Marking[P]] =
    m =>
      for
        Trn(cond, rate, eff, inh) <- spn
        if m disjointed inh
        r = rate(m)
        out <- m extract cond
      yield Action(r, out union eff)

  def apply[P](transitions: Trn[P]*): SPN[P] = transitions.toSet

  extension [P](self: Marking[P])
    @targetName("transitionWithRate")
    infix def ~~(rate: Marking[P] => Double): Trn[P] = Trn(self, rate, \(), \())
  extension [P](self: Trn[P])
    @targetName("transitionTo")
    infix def ~~>(y: Marking[P]): Trn[P] = self.copy(eff = y)
    @targetName("inhibitedBy")
    infix def ^^^(z: Marking[P]): Trn[P] = self.copy(inh = z)
