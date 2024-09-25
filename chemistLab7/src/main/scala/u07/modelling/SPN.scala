package u07.modelling

import u06.utils.MSet
import u07.modelling.CTMC.*
import scala.annotation.targetName

object SPN:

  /**
   * Case class representing a transition in a Stochastic Petri Net (SPN).
   *
   * @param cond
   *   The pre-condition marking (tokens required for the transition to fire).
   * @param rate
   *   A function that takes the current marking and returns the firing rate of the transition.
   * @param eff
   *   The effect marking (tokens produced by the transition).
   * @param inh
   *   The inhibition marking (tokens that inhibit the transition from firing).
   * @tparam P
   *   The type of places in the Petri net.
   */
  case class Trn[P](
    cond: Marking[P],
    rate: Marking[P] => Double,
    eff: Marking[P],
    inh: Marking[P]
  )

  /**
   * Type alias for a Stochastic Petri Net (SPN).
   *
   * An SPN is represented as a set of transitions (`Trn`).
   * @tparam P
   *   The type of places in the Petri net.
   */
  type SPN[P] = Set[Trn[P]]

  /**
   * Type alias for a marking in the Petri net, represented as a multiset of places.
   *
   * A marking is a distribution of tokens over places in the Petri net.
   * @tparam P
   *   The type of places in the Petri net.
   */
  type Marking[P] = MSet[P]

  import u06.verifier.util.PetriNets.\

  /**
   * Converts a Stochastic Petri Net (SPN) to a Continuous-Time Markov Chain (CTMC).
   *
   * This function takes an SPN and transforms it into a CTMC by defining the transitions and rates between markings.
   *
   * @param spn
   *   The SPN to be converted.
   * @tparam P
   *   The type of places in the Petri net.
   * @return
   *   A CTMC where states are markings (multisets of places).
   */
  def toCTMC[P](spn: SPN[P]): CTMC[Marking[P]] =
    m =>
      for
        Trn(cond, rate, eff, inh) <- spn
        if m disjointed inh // The transition can only fire if the marking is disjoint from the inhibition marking.
        r = rate(m) // Calculate the rate for the current marking.
        out <- m extract cond // Try to extract the tokens for the transition.
      yield Action(r, out union eff) // Return an action with the rate and the new marking.

  /**
   * Factory method for creating a Stochastic Petri Net (SPN) from a sequence of transitions.
   *
   * @param transitions
   *   The transitions that define the SPN.
   * @tparam P
   *   The type of places in the Petri net.
   * @return
   *   A set of transitions forming the SPN.
   */
  def apply[P](transitions: Trn[P]*): SPN[P] = transitions.toSet

  // Extensions to define more expressive syntax for constructing transitions.

  extension [P](self: Marking[P])
    /**
     * Extension method for `Marking`, allowing the definition of a transition with a firing rate.
     * @param rate
     *   A function that returns the rate of the transition based on the current marking.
     * @return
     *   A transition with the specified pre-condition marking and firing rate.
     */
    @targetName("transitionWithRate")
    infix def ~~(rate: Marking[P] => Double): Trn[P] = Trn(self, rate, \(), \())

  extension [P](self: Trn[P])
    /**
     * Extension method for `Trn`, allowing the definition of the effect of a transition.
     *
     * @param y
     *   The effect marking (tokens produced by the transition).
     * @return
     *   A transition with the specified effect marking.
     */
    @targetName("transitionTo")
    infix def ~~>(y: Marking[P]): Trn[P] = self.copy(eff = y)

  extension [P](self: Trn[P])
    /**
     * Extension method for `Trn`, allowing the definition of an inhibition condition for a transition.
     *
     * @param z
     *   The inhibition marking (tokens that inhibit the transition).
     * @return
     *   A transition with the specified inhibition marking.
     */
    @targetName("inhibitedBy")
    infix def ^^^(z: Marking[P]): Trn[P] = self.copy(inh = z)
