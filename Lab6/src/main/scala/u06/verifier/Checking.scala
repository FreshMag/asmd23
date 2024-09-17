package u06.verifier

object Checking:
  import u06.verifier.PNChecking.hasTokens
  import u06.modelling.PetriNet.Marking
  import u06.modelling.SystemAnalysis.*
  import u06.modelling.System

  type Limit = Int
  type Paths[T] = Seq[Path[T]]
  type ReachedDef[T] = T => T => Boolean

  given markingReachedDefinition[T]: ReachedDef[Marking[T]] = hasTokens

  extension [T](x: System[T])
    infix def satisfies(block: System[T] ?=> Boolean): Boolean =
      given System[T] = x
      block
      
  inline def system[T](using s: System[T]): System[T] = s

  def E[T, C](searchSpace: System[T] => Iterable[C])(condition: C => Boolean)(using s: System[T]): Boolean =
    searchSpace(s).exists(condition)

  def notE[T, C](searchSpace: System[T] => Iterable[C])(condition: C => Boolean)(using s: System[T]): Boolean =
    !E(searchSpace)(condition)(using s)
    
  def next[T](node: T)(using s: System[T]): System[T] => Set[T] = _ => s.next(node)

  def reachable[T](destination: T)(using s: System[T])(using l: Limit)(using
    countAsHaveReached: ReachedDef[T]
  ): T => Boolean =
    startPoint => E(path from startPoint).suchThat(_.hasMarkingThat(countAsHaveReached(destination)))

  def path[T](using limit: Limit): T => System[T] => Paths[T] =
    m => s => (1 to limit).to(LazyList) flatMap (s.paths(m, _))

  extension [T, Result](x: T => System[T] => Result) infix def from(p: T): System[T] => Result = s => x(p)(s)

  extension [T, Result](x: T => Result) infix def from(p: T): Result = x(p)

  extension [C](x: PartialFunction[C => Boolean, Boolean])
    infix def suchThat(condition: C => Boolean): Boolean = x(condition)

  extension [T](x: Path[T])
    infix def hasMarkingThat(condition: T => Boolean): Boolean =
      x.exists(condition)
