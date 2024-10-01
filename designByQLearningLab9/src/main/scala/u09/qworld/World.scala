package u09.qworld

object World:
  enum Move:
    case LEFT, RIGHT, UP, DOWN, INTERACT_UP, INTERACT_DOWN, INTERACT_LEFT,
      INTERACT_RIGHT, IDLE

    override def toString: String = this match
      case Move.LEFT => "<"
      case Move.RIGHT => ">"
      case Move.UP => "^"
      case Move.DOWN => "v"
      case Move.INTERACT_LEFT => "<*"
      case Move.INTERACT_RIGHT => "*>"
      case Move.INTERACT_UP => "*^"
      case Move.INTERACT_DOWN => "*v"
      case Move.IDLE => "..."

  import Move.*

  object Nodes:

    trait Node:
      def actWith(move: Move): (Option[Position], Double, Node)
      def position: Position

    abstract class AbstractNode(private val _position: Position) extends Node:
      override def actWith(move: Move): (Option[Position], Double, Node)
      override def position: Position = _position

    case class RewardNode(private val reward: Double, private val _position: Position)
        extends AbstractNode(_position):
      override def actWith(move: Move): (Option[Position], Double, Node) = move match
        case LEFT | RIGHT | UP | DOWN => (Some(position), reward, this)
        case _ => (None, -1, this)

    case class BarrierNode(private val _position: Position) extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double, Node) = (None, -1, this)

    case class ExtractionNode(private var resourceReward: Double, private val _position: Position)
        extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double, Node) =
        val result = move match
          case INTERACT_LEFT | INTERACT_RIGHT | INTERACT_UP | INTERACT_DOWN =>
            (None, resourceReward, this.copy((resourceReward - 1) max 0))
          case _ => (None, -1.0, this)
        result

  import Nodes.*

  type Position = (Int, Int)
  type World = List[List[Node]]
  type State = (World, Position)

  def availableActions(state: State): Set[Move] =
    val (row, col) = state._2
    val world = state._1

    Move.values.map:
      case move @ (LEFT | INTERACT_LEFT) => (move, (row, col - 1))
      case move @ (RIGHT | INTERACT_RIGHT) => (move, (row, col + 1))
      case move @ (UP | INTERACT_UP) => (move, (row - 1, col))
      case move @ (DOWN | INTERACT_DOWN) => (move, (row + 1, col))
      case move => (move, (row, col))
    .filter:
      case (_, (newRow, newCol))
        if newRow >= 0 && newCol >= 0 && newRow < world.length && newCol < world(newRow).length && !world(newRow)(
          newCol
        ).isInstanceOf[BarrierNode] => true
      case _ => false
    .map(_._1)
      .toSet

  def move(state: State, action: Move): (Double, State) =
    val actTarget = (state, action) match
      case ((world, (row, column)), UP | INTERACT_UP) if row - 1 >= 0 && world(row - 1).length > column =>
        (row - 1, column)
      case ((world, (row, column)), DOWN | INTERACT_DOWN)
        if row + 1 < world.length && world(row + 1).length > column =>
        (row + 1, column)
      case ((world, (row, column)), RIGHT | INTERACT_RIGHT) if column + 1 < world(row).length =>
        (row, column + 1)
      case ((_, (row, column)), LEFT | INTERACT_LEFT) if column - 1 >= 0 => (row, column - 1)
      case ((_, (row, column)), _) => (row, column)

    (actTarget, action) match
      case (state._2, IDLE) => (0, state)
      case (state._2, _) => (Double.MinValue, state) // illegal move
      case _ =>
        val (targetRow, targetCol) = actTarget
        val (prevWorld, _) = state
        val (newPositionOption, reward, updatedNode) = prevWorld(targetRow)(targetCol).actWith(action)
        newPositionOption match
          case Some((destRow, destCol)) =>
            val newWorld = prevWorld.updated(destRow, prevWorld(destRow).updated(destCol, updatedNode))
            (reward, (newWorld, (destRow, destCol)))
          case None => (reward, state)
