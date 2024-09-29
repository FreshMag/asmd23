package u09.qworld

import u09.qworld.QMatrix.Facade

object TryQWorld extends App :
  import u09.qworld.World.Nodes.*

  val worldShape: World.World = List(
    List(BasicNode(0, (0,0)), BlockNode(10, (0, 1)), BasicNode(0, (0, 2))),
    List(BasicNode(10, (1,0)), BarrierNode((1,1)), BasicNode(0, (1, 2))),
    List(BarrierNode((2,0)), BasicNode(0, (2,1)),  BlockNode(0, (2, 2)))
  )


  val rl: QMatrix.Facade = Facade(
    initial = (worldShape, (2,1)),
    terminal = {case _ => false},
    gamma = 0.5,
    alpha = 0.5,
    epsilon = 0.5,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(worldShape, q0.vFunction,"%2.2f"))
  val q1 = rl.makeLearningInstance().learn(10000,100,q0)
  println(rl.show(worldShape, q1.vFunction,"%2.2f"))
  println(rl.show(worldShape, s => q1.bestPolicy(s).toString,"%7s"))