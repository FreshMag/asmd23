package u04.mvc

import u04.monads.States.State

object MonadicMVC:
  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State:
      case (sm, sv) =>
        val (sm2, am) = m1.run(sm)
        val (sv2, av) = f(am).run(sv)
        ((sm2, sv2), av)
