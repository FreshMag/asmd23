package u07.modelling

/**
 * Continuous time Markov chains abstraction
 * @tparam S
 *   type of the represented state
 */
trait CTMC[S]:
  import CTMC.Action

  /**
   * Returns the possible actions from the selected state
   * @param a
   *   current state
   * @return
   *   the possible actions.
   */
  def transitions(a: S): Set[Action[S]] // rate + state

/**
 * Implementation of CTMCs.
 */
object CTMC:

  /**
   * Represents an action inside a CTMC
   * @param rate
   *   rate of the action
   * @param state
   *   the arrival state of the transition
   * @tparam S
   *   type of the state of the CTMC
   */
  case class Action[S](rate: Double, state: S)
  extension [S](rate: Double)
    /**
     * Builds an [[Action]]
     * @param state
     *   arrival state
     * @return
     *   an [[Action]] with the selected state and rate
     */
    def -->(state: S) = Action(rate, state)

  /**
   * Represents a transition.
   * @param state
   *   starting state
   * @param action
   *   action performed
   * @tparam S
   *   type of the state of the CTMC
   */
  case class Transition[S](state: S, action: Action[S])

  /**
   * Utility method to build a CTMC from a function
   * @param f
   *   starting function
   * @tparam S
   *   type of the CTMC
   * @return
   *   a [[CTMC]]
   */
  def ofFunction[S](f: PartialFunction[S, Set[Action[S]]]): CTMC[S] =
    s => f.applyOrElse(s, x => Set[Action[S]]())

  /**
   * Utility method to build a CTMC from a [[Set]] of [[Transition]]s
   * @param rel
   *   relations
   * @tparam S
   *   type of the state of the CTMC
   * @return
   *   a [[CTMC]]
   */
  def ofRelation[S](rel: Set[Transition[S]]): CTMC[S] =
    ofFunction(s => rel filter (_.state == s) map (_.action))

  /**
   * Utility method to build a CTMC from a [[Set]] of [[Transition]]s
   * @param rel
   *   relations
   * @tparam S
   *   type of the state of the CTMC
   * @return
   *   a [[CTMC]]
   */
  def ofTransitions[S](rel: Transition[S]*): CTMC[S] = ofRelation(rel.toSet)
