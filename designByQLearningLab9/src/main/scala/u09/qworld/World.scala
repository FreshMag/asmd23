package u09.qworld

object World:
  enum Move:
    case LEFT, RIGHT, UP, DOWN, JUMP_LEFT, JUMP_RIGHT, JUMP_UP, JUMP_DOWN, INTERACT

    override def toString: String = this match
      case Move.LEFT => "<"
      case Move.RIGHT => ">"
      case Move.UP => "^"
      case Move.DOWN => "v"
      case Move.JUMP_LEFT => "<J"
      case Move.JUMP_RIGHT => "J>"
      case Move.JUMP_UP => "J^"
      case Move.JUMP_DOWN => "Jv"
      case Move.INTERACT => "*"

  import Move.*

  object Nodes:

    trait Node:
      def onArrive(world: World): Node
      def actWith(move: Move): (Option[Position], Double)
      def position: Position

    abstract class AbstractNode(private val _position: Position) extends Node:
      override def onArrive(world: World): Node = this
      override def actWith(move: Move): (Option[Position], Double)
      override def position: Position = _position

    case class RewardNode(private val reward: Double, private val _position: Position)
        extends AbstractNode(_position):
      override def actWith(move: Move): (Option[Position], Double) = move match
        case LEFT | RIGHT | UP | DOWN => (Some(position), reward)
        case _ => (None, 0)

    case class BlockNode(private val reward: Double, private val _position: Position)
        extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double) = move match
        case JUMP_LEFT | JUMP_RIGHT | JUMP_UP | JUMP_DOWN => (Some(position), reward)
        case _ => (None, 0)

    case class BarrierNode(private val _position: Position) extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double) = (None, -1)

  import Nodes.*

  type Position = (Int, Int)
  type World = List[List[Node]]
  type State = (World, Position)

  def move(state: State, action: Move): (Double, State) =
    val actTarget = (state, action) match
      case ((world, (row, column)), UP | JUMP_UP) if row - 1 >= 0 && world(row - 1).length > column => (row - 1, column)
      case ((world, (row, column)), DOWN | JUMP_DOWN) if row + 1 < world.length && world(row + 1).length > column =>
        (row + 1, column)
      case ((world, (row, column)), RIGHT | JUMP_RIGHT) if column + 1 < world(row).length => (row, column + 1)
      case ((_, (row, column)), LEFT | JUMP_LEFT) if column - 1 >= 0 => (row, column - 1)
      case ((_, (row, column)), _) => (row, column)

    val (targetRow, targetCol) = actTarget
    val (prevWorld, _) = state
    val (newPositionOption, reward) = prevWorld(targetRow)(targetCol).actWith(action)
    newPositionOption match
      case Some((destRow, destCol)) =>
        val newNode = prevWorld(destRow)(destCol).onArrive(prevWorld)
        val newWorld = prevWorld.updated(destRow, prevWorld(destRow).updated(destCol, newNode))
        (reward, (newWorld, (destRow, destCol)))
      case None => (reward, state)
