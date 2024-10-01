package u09.qworld.utils

import u09.qworld.World.Nodes.BarrierNode

object ConsoleUtility:
  val ANSI_RESET = "\u001B[0m"
  val ANSI_BLACK: String = "\u001B[30m"
  val ANSI_RED: String = "\u001B[31m"
  val ANSI_GREEN: String = "\u001B[32m"
  val ANSI_YELLOW: String = "\u001B[33m"
  val ANSI_BLUE: String = "\u001B[34m"
  val ANSI_PURPLE: String = "\u001B[35m"
  val ANSI_CYAN: String = "\u001B[36m"
  val ANSI_WHITE: String = "\u001B[37m"

  val ANSI_BLACK_BACKGROUND = "\u001B[40m"
  val ANSI_RED_BACKGROUND: String = "\u001B[41m"
  val ANSI_GREEN_BACKGROUND: String = "\u001B[42m"
  val ANSI_YELLOW_BACKGROUND: String = "\u001B[43m"
  val ANSI_BLUE_BACKGROUND: String = "\u001B[44m"
  val ANSI_PURPLE_BACKGROUND: String = "\u001B[45m"
  val ANSI_CYAN_BACKGROUND: String = "\u001B[46m"
  val ANSI_WHITE_BACKGROUND: String = "\u001B[47m"

  import u09.qworld.World.State

  def toConsole[E](toShow: State => E, formatting: String, barrierNode: E): State => String =
    case state@(world, (row, col)) =>
      if world(row)(col).isInstanceOf[BarrierNode] then
        ANSI_WHITE_BACKGROUND + formatting.format(barrierNode) + ANSI_RESET
      else
        formatting.format(toShow(state))
