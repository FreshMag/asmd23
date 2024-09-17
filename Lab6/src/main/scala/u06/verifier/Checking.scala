package u06.verifier


/**
 * Collection of methods to check properties on [[u06.modelling.System]]s and [[PetriNet]]s
 */
object Checking:
  import u06.modelling.PetriNet.Trn
  import u06.verifier.PNChecking.hasTokens
  import u06.modelling.PetriNet.Marking
  import u06.modelling.SystemAnalysis.*
  import u06.utils.MSet
  import u06.modelling.System

  /**
   * A type alias representing a limit (Int). Used to limit the depth of search algorithms.
   */
  type Limit = Int

  /**
   * A type alias for a sequence of paths through a system.
   * @tparam T
   *   - the state or node type of the system.
   */
  type Paths[T] = Seq[Path[T]]

  /**
   * A type alias for a function that checks if one state has been "reached" from another.
   * @tparam T
   *   - the state or node type of the system.
   */
  type ReachedDef[T] = T => T => Boolean

  /**
   * A given instance that defines how to determine if a marking (state) has been reached. Uses the hasTokens function
   * to check if a state has the required tokens.
   * @tparam T
   *   - the type of tokens or markings used in the system.
   */
  given markingReachedDefinition[T]: ReachedDef[Marking[T]] = hasTokens

  extension [T](x: System[T])
    /**
     * Extension method for systems that allows checking if a system satisfies a given condition.
     * @param block
     *   the condition to check within the context of the system.
     * @return
     *   true if the system satisfies the condition, false otherwise.
     */
    infix def satisfies(block: System[T] ?=> Boolean): Boolean =
      given System[T] = x
      block

  /**
   * Inline helper function to get the current system instance in the context.
   * @tparam T
   *   the type of the system's state.
   * @param s
   *   the current system.
   * @return
   *   the system itself.
   */
  inline def system[T](using s: System[T]): System[T] = s

  /**
   * Performs an existential check over a search space. Checks if there exists an element in the search space that
   * satisfies the given condition.
   * @tparam T
   *   the type of the system's state.
   * @tparam C
   *   the type of the elements in the search space.
   * @param searchSpace
   *   the function that generates the search space from the system.
   * @param condition
   *   the condition to check on elements of the search space.
   * @param s
   *   the current system.
   * @return
   *   true if there exists an element that satisfies the condition.
   */
  def E[T, C](searchSpace: System[T] => Iterable[C])(condition: C => Boolean)(using s: System[T]): Boolean =
    searchSpace(s).exists(condition)

  /**
   * Negates the result of the existential check [[E]]. Returns true if no element in the search space satisfies the
   * condition.
   * @tparam T
   *   the type of the system's state.
   * @tparam C
   *   the type of the elements in the search space.
   * @param searchSpace
   *   the function that generates the search space from the system.
   * @param condition
   *   the condition to check on elements of the search space.
   * @param s
   *   the current system.
   * @return
   *   true if no element satisfies the condition, false otherwise.
   */
  def notE[T, C](searchSpace: System[T] => Iterable[C])(condition: C => Boolean)(using s: System[T]): Boolean =
    !E(searchSpace)(condition)(using s)

  /**
   * Returns the set of next possible states from a given node in the system.
   * @tparam T
   *   the type of the system's state.
   * @param node
   *   the current node in the system.
   * @param s
   *   the current system.
   * @return
   *   a function that, given a system, returns the set of next possible states.
   */
  def next[T](node: T)(using s: System[T]): System[T] => Set[T] = _ => s.next(node)

  /**
   * Determines whether a destination state is reachable from a start point within a given limit.
   * @tparam T
   *   the type of the system's state.
   * @param destination
   *   the target state.
   * @param s
   *   the current system.
   * @param countAsHaveReached
   *   the function to determine if the destination has been reached.
   * @return
   *   a function that checks if the startPoint can reach the destination.
   */
  def reachable[T](destination: T)(using s: System[T])(using Limit)(using
    countAsHaveReached: ReachedDef[T]
  ): T => Boolean =
    startPoint => E(path from startPoint).suchThat(_.hasMarkingThat(countAsHaveReached(destination)))

  /**
   * Checks whether a transition of the Petri net is L1-live (i.e. it is in some firing sequence)
   * @param transition
   *   transition to check.
   * @param s
   *   given [[System]]
   * @tparam T
   *   type of the [[Marking]] of this Petri net
   * @return
   *   a function that, given a [[Marking]], tells if the transition is L1-live starting from that marking
   */
  def livenessL1[T](transition: Trn[T])(using s: System[Marking[T]])(using Limit): Marking[T] => Boolean =
    if s.next(transition.cond).contains(transition.eff) then
      startPoint => (path from startPoint)(s).exists(_.sliding(2).filter(_.size == 2).exists:
        case List(first, second) if hasTokens(transition.cond)(first) && hasTokens(transition.eff)(second) => true
        case _ => false
      )
    else _ => false

  /**
   * Returns the paths from a given starting point in the system, up to a given limit.
   * @tparam T
   *   the type of the system's state.
   * @param limit
   *   the maximum depth of the search.
   * @return
   *   a sequence of paths from the start point.
   */
  def path[T](using limit: Limit): T => System[T] => Paths[T] =
    m => s => (1 to limit).to(LazyList) flatMap (s.paths(m, _))

  extension [T, Result](x: T => System[T] => Result)
    /**
     * Extension method to call a function from a starting point in the system (e.g. `path from (...)`)
     * @param p
     *   the starting point.
     * @return
     *   the result produced by the function when executed on the system.
     */
    infix def from(p: T): System[T] => Result = s => x(p)(s)

  extension [T, Result](x: T => Result)
    /**
     * Extension method to directly execute a function from a starting point (e.g. `reachable(...) from`)
     * @param p
     *   the starting point.
     * @return
     *   the result produced by the function.
     */
    infix def from(p: T): Result = x(p)

  extension [C](x: PartialFunction[C => Boolean, Boolean])
    /**
     * Extension method for calling a [[PartialFunction]] that accepts a condition in a more idiomatic way.
     * @param condition
     *   the condition to check.
     * @return
     *   true if the condition is satisfied, false otherwise.
     */
    infix def suchThat(condition: C => Boolean): Boolean = x(condition)

  extension [T](x: Path[T])
    /**
     * Extension method to check if a [[Path]] has a marking that satisfies a condition.
     * @param condition
     *   the condition to check on the marking.
     * @return
     *   true if any marking in the path satisfies the condition, false otherwise.
     */
    infix def hasMarkingThat(condition: T => Boolean): Boolean =
      x.exists(condition)
