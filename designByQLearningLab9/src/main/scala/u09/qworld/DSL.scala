package u09.qworld

import u09.qworld.World.Nodes.{BarrierNode, BlockNode, Node, RewardNode}
import u09.qworld.World.Position

import scala.annotation.targetName

object DSL:

  extension (nodeSetter: Position => Node)
    @targetName("initRow")
    infix def |(otherSetter: Position => Node): Int => Row =
      rowIndex =>
        Row(rowIndex, List(nodeSetter((rowIndex, 0)), otherSetter((rowIndex, 1))))

  extension (rowSetter: Int => Row)
    @targetName("addToRow")
    infix def |(otherSetter: Position => Node): Int => Row =
      rowIndex =>
        val row = rowSetter(rowIndex)
        row.copy(nodes = row.nodes.:+(otherSetter((rowIndex, row.nodes.length))))

  case class Row(index: Int, nodes: List[Node])

  def world(rowSetters: Int => Row*): World.World =
    rowSetters.zipWithIndex.map: (rowSetter, index) =>
      rowSetter(index).nodes
    .toList

  given nodeToRowSetter: Conversion[Position => Node, Int => Row] =
    nodeSetter =>
      rowIndex =>
        Row(rowIndex, List(nodeSetter((rowIndex, 0))))

  def $(money: Double): Position => Node = RewardNode(money, _)
  def o: Position => Node = RewardNode(0, _)
  def / : Position => Node = BarrierNode(_)
  def __ : Position => Node = BlockNode(0, _)
  def _$(money: Double): Position => Node = BlockNode(money, _)
