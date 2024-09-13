package scala.u04.mvc.engineer

import u04.datastructures.Streams.Stream
import u04.monads.States.State.*
import u04.monads.States.State
import u04.monads.Monads.*
import u04.monads.Monads.Monad.*

import scala.concurrent.duration.FiniteDuration
import scala.u04.mvc.engineer.BallModel.GameStateImpl.*
import scala.u04.mvc.engineer.BallView.WindowStateImpl
import scala.u04.mvc.engineer.BallView.WindowStateImpl.Window


object BallController:
  
  trait Controller:
    type View
    type Model
    type ModelOut
    type Event

    def gameLoop(
      events: Stream[Event],
      updateM: State[Model, ModelOut],
      updateV: ModelOut => State[View, Unit],
      period: FiniteDuration
    ): State[(Model, View), Unit]
    
  object ControllerImpl extends Controller:
    opaque type View = Window
    opaque type Model = Game
    opaque type Event = String
    opaque type ModelOut = BallPosition

    def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
      State:
        case (sm, sv) =>
          val (sm2, am) = m1.run(sm)
          val (sv2, av) = f(am).run(sv)
          ((sm2, sv2), av)
    
    private def loop(period: FiniteDuration): State[Window, Unit] =
      State(w => (w.schedule(period.toMillis.toInt, "Loop"), ()))

    override def gameLoop(
         events: Stream[String],
         updateM: State[Game, BallPosition],
         updateV: BallPosition => State[Window, Unit],
         period: FiniteDuration
   ): State[(Game, Window), Unit] = 
      for
        _ <- mv(nop(), _ => loop(period))
        _ <- seqN(events.map:
          case "Loop" => mv(updateM, ball => seq(updateV(ball), loop(period)))
        )
      yield ()
    
    
    
    
    
