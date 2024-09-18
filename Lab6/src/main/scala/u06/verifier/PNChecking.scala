package u06.verifier

object PNChecking:
  import u06.modelling.System
  import u06.modelling.PetriNet.Marking
  import u06.utils.MSet
  export KMTrees.{KMTree, buildKarpMillerTree, containsOmegaMarking}

  /**
   * Checks if a given marking [[markingThatShouldContainTokens]] contains all the tokens of marking
   * [[markingWithTokens]].
   * @param markingWithTokens
   *   marking with the tokens to check
   * @param markingThatShouldContainTokens
   *   marking to check if it contains the requested tokens
   * @tparam T
   *   type of [[Marking]]
   * @return
   *   true if [[markingThatShouldContainTokens]] contains all the tokens of [[markingWithTokens]], false otherwise
   */
  def hasTokens[T](markingWithTokens: Marking[T])(markingThatShouldContainTokens: Marking[T]): Boolean =
    (markingWithTokens diff markingThatShouldContainTokens).size == 0

  /**
   * Collection of useful methods to build and analyze Karp-Miller trees.
   */
  private object KMTrees:
    /**
     * A class representing a node in the Karp-Miller tree.
     * @param marking
     *   the marking (state) at this node.
     * @param children
     *   the children nodes in the tree (corresponding to possible next markings).
     */
    case class KMTree[T](marking: Marking[T], children: Seq[KMTree[T]])

    /**
     * Recursively builds the Karp-Miller tree for the system.
     * @tparam T
     *   the type of the system's state.
     * @param system
     *   the system to explore.
     * @param marking
     *   the current marking (state).
     * @param visited
     *   the set of markings already visited to prevent loops.
     * @return
     *   the root node of the constructed Karp-Miller tree.
     */
    def buildKarpMillerTree[T](system: System[Marking[T]], marking: Marking[T], visited: Set[Marking[T]]): KMTree[T] =
      if visited.contains(marking) then
        KMTree(omega(marking), Seq.empty)
      else
        val children = system.next(marking).map(buildKarpMillerTree(system, _, visited + marking))
        KMTree(marking, children.toSeq)

    /**
     * Checks if any node in the Karp-Miller tree contains an omega marking.
     * @tparam T
     *   the type of the system's state.
     * @param tree
     *   the root node of the Karp-Miller tree.
     * @return
     *   true if the tree contains an omega marking, false otherwise.
     */
    def containsOmegaMarking[T](tree: KMTree[T]): Boolean =
      if isOmega(tree.marking) then true
      else tree.children.exists(containsOmegaMarking)

    /**
     * Creates an omega marking, which indicates unbounded behavior.
     * @param marking
     *   the current marking.
     * @return
     *   a marking with at least one unbounded place (represented by an "omega").
     */
    private def omega[T](marking: Marking[T]): Marking[T] =
      marking.map:
        case (place, count) => (place, if count > 0 then Int.MaxValue else 0)

    /**
     * Checks if a marking contains any unbounded place (i.e., if it's an omega marking).
     * @param marking
     *   the marking to check.
     * @return
     *   true if the marking is unbounded, false otherwise.
     */
    private def isOmega[T](marking: Marking[T]): Boolean =
      marking.asMap.values.exists(_ == Int.MaxValue)
