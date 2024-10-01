package u09.examples

import TwoWaysMDP.*

object TryRL extends App:
  val rl = TwoWaysMDP.rl.rlTW()
  var qf = rl.learn(20, 10, TwoWaysMDP.rl.qfTW())

  for
    i <- -5 to 10
  do
    println:
      (i, qf.actions(Pos(i)).maxBy(qf(Pos(i), _)), qf.actions(Pos(i)).map(qf(Pos(i), _)).max)
