package u09.qworld

import u09.qworld.QMatrix.Facade
import u09.utils.ConsoleUtility.toConsole

object TryQWorld extends App:
  import DSL.*

  val worldShape: World.World = world(
    /     | $(-1)     | /      ,
    $(-1) | $(0)     | $(-5)   ,
    $(-1) | $(-5)     | $(9)   ,
    o     | ?(3)
  )

  val rl: QMatrix.Facade = Facade(
    initial = (worldShape, (0, 1)),
    terminal = { case _ => false },
    gamma = 0.6,
    alpha = 0.5,
    epsilon = 0.5,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(worldShape, toConsole(q0.vFunction, "%2.2f", rl.v0)))
  val q1 = rl.makeLearningInstance().learn(10000, 100, q0)
  println(rl.show(worldShape, toConsole(q0.vFunction, "%2.2f", 0.0)))
  println(rl.show(worldShape, toConsole(s => q1.bestPolicy(s).toString, "%7s", "")))
