package u09.qworld

import u09.qworld.QMatrix.Facade

object TryQWorld extends App :
  import DSL.*

  val worldShape: World.World = world(
    /     | o      | $(1)  ,
    /     | o      | o     ,
    o     | _$(5)  | /     ,
    $(10)
  )


  val rl: QMatrix.Facade = Facade(
    initial = (worldShape, (0,1)),
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