package u09.qworld

import u09.model.QRLImpl
import u09.utils.ConsoleColors

object QMatrix:

  import World.Move

  case class Facade(
    initial: World.State,
    terminal: PartialFunction[World.State, Boolean],
    gamma: Double,
    alpha: Double,
    epsilon: Double = 0.0,
    v0: Double
  ) extends QRLImpl:
    type State = World.State
    type Action = Move

    def qEnvironment(): Environment = (s: State, a: Move) =>
      World.move(s, a)

    def qFunction: QFunction = QFunction(Move.values.toSet, v0, terminal)
    def qSystem: QSystem = QSystem(environment = qEnvironment(), initial, terminal)
    def makeLearningInstance(): QLearning = QLearning(qSystem, gamma, alpha, epsilon, qFunction)

    def show[E](worldShape: World.World, v: State => E, formatString: String): String =
      (for
        row <- worldShape.indices
        col <- worldShape(row).indices
      yield
        (if ((row, col) == initial._2) then ConsoleColors.ANSI_RED else "") +
          formatString.format(v((worldShape, (row, col)))) +
          (if (col == worldShape(row).length - 1) "\n" else "\t") + ConsoleColors.ANSI_RESET)
        .mkString("")
