package u09.utils

import scala.util.Random

object Stochastics:

  given Random = new Random()

  export u07.utils.Stochastics.*

  def drawFiltered(filter: Double => Boolean)(using rnd: Random): Boolean =
    filter(rnd.nextDouble())

  def uniformDraw[A](actions: Set[A])(using rnd: Random): A =
    actions.toList(rnd.nextInt(actions.size))
