package u09.examples

import u09.qworld.DSL
import u09.qworld.DSL.*
import u09.qworld.utils.RLInstance

object TryQWorld extends RLInstance(
  world = world (
    /     | $(-1)    | /      ,
    $(-1) | $(0)     | $(-5)  ,
    $(-1) | $(-5)    | $(9)   ,
    o     | ?(3)
  ),
  initialPosition = (0, 1),
  isTerminal = _ => false,
  gamma = 0.6,
  alpha = 0.5,
  epsilon = 0.5,
  showQValues = true
)
