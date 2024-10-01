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

  private[qworld] object Nodes:

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
        case _ => (None, 0.0, this)

    case class BarrierNode(private val _position: Position) extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double, Node) = (None, -1, this)

    case class ExtractionNode(private var resourceReward: Double, private val _position: Position)
        extends AbstractNode(_position):
      override def actWith(move: Move): (Option[(Int, Int)], Double, Node) =
        val result = move match
          case INTERACT_LEFT | INTERACT_RIGHT | INTERACT_UP | INTERACT_DOWN =>
            (None, resourceReward, copy((resourceReward - 1) max 0.0))
          case _ => (None, 0.0, this)
        result

  import Nodes.*

  type Position = (Int, Int)
  type World = List[List[Node]]
  type State = (World, Position)
  
  private def getTarget(move: Move)(position: Position): Position = 
    val (row, col) = position
    move match
      case LEFT | INTERACT_LEFT => (row, col - 1)
      case RIGHT | INTERACT_RIGHT => (row, col + 1)
      case UP | INTERACT_UP => (row - 1, col)
      case DOWN | INTERACT_DOWN => (row + 1, col)
      case _ => (row, col)
      
  private def isValidPosition(position: Position, world: World): Boolean = 
    position match
      case (newRow, newCol)
        if newRow >= 0 && newCol >= 0 && newRow < world.length && newCol < world(newRow).length && !world(newRow)(
          newCol
        ).isInstanceOf[BarrierNode] => true
      case _ => false
  
  def availableActions(state: State): Set[Move] =
    val (row, col) = state._2
    val world = state._1

    Move.values
      .map(move => (move, getTarget(move)((row, col))))
      .filter((_, position) => isValidPosition(position, world))
      .map(_._1)
      .toSet

  def move(state: State, action: Move): (Double, State) =
    val (prevWorld, prevPos) = state
    val actTarget = getTarget(action)(prevPos)

    (actTarget, action) match
      case (_, IDLE) => (0, state) // remaining idle cause no effect and zero reward
      case (pos, _) if !isValidPosition(pos, prevWorld) => (Double.MinValue, state) // illegal move
      case _ => // the agent performed a valid move 
        val (targetRow, targetCol) = actTarget
        val (newPositionOption, reward, updatedNode) = prevWorld(targetRow)(targetCol).actWith(action)
        newPositionOption match
          case Some((destRow, destCol)) => // the agent moved into a new position
            val newWorld = prevWorld.updated(destRow, prevWorld(destRow).updated(destCol, updatedNode))
            (reward, (newWorld, (destRow, destCol)))
          case None => (reward, state) // the agent stayed in the same position
