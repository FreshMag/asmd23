/**
 * Modified version of the original `MSet.scala` from Lab6
 */
package u06.utils

/**
 * A multiset datatype
 */
trait MSet[A] extends (A => Int):
  infix def union(m: MSet[A]): MSet[A]
  infix def diff(m: MSet[A]): MSet[A]
  infix def disjointed(m: MSet[A]): Boolean
  def size: Int
  infix def matches(m: MSet[A]): Boolean
  infix def extract(m: MSet[A]): Option[MSet[A]]
  def asList: List[A]
  def asMap: Map[A, Int]
  def iterator: Iterator[A]

/**
 * Functional-style helpers/implementation
 */
object MSet:
  // Factories
  def apply[A](l: A*): MSet[A] = new MSetImpl(l.toList)
  def ofList[A](l: List[A]): MSet[A] = new MSetImpl(l)
  def ofMap[A](m: Map[A, Int]): MSet[A] = MSetImpl(m)

  // Hidden reference implementation
  private case class MSetImpl[A](asMap: Map[A, Int]) extends MSet[A]:
    def this(list: List[A]) = this(list.groupBy(a => a).map((a, n) => (a, n.size)))
    override val asList: List[A] =
      asMap.toList.flatMap((a, n) => List.fill(n)(a))

    override infix def apply(v1: A): Int = asMap.getOrElse(v1, 0)
    override infix def union(m: MSet[A]) = new MSetImpl[A](asList ++ m.asList)
    override infix def diff(m: MSet[A]) = new MSetImpl[A](asList diff m.asList)
    override infix def disjointed(m: MSet[A]): Boolean = (asList intersect m.asList).isEmpty
    override def size: Int = asList.size
    override infix def matches(m: MSet[A]): Boolean = extract(m).isDefined
    override infix def extract(m: MSet[A]): Option[MSet[A]] =
      Some(this diff m) filter (_.size == size - m.size)
    override def iterator: Iterator[A] = asMap.keysIterator
    override def toString = s"{${asList.mkString("|")}}"
