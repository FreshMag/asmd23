package scala.u04.mvc.engineer

import u04.datastructures.Streams.Stream
import u04.monads.States.State.*
import u04.monads.States.State
import u04.monads.Monads.*
import u04.monads.Monads.Monad.*

import scala.concurrent.duration.FiniteDuration
import scala.u04.mvc.engineer.BallModel.GameStateImpl.*
import scala.u04.mvc.engineer.BallView.WindowStateImpl.*
import scala.u04.mvc.engineer.BallController.ControllerImpl.*
import scala.u04.mvc.engineer.BallController.ControllerImpl.Event


@main def ballGame(): Unit =
  
  def windowCreation(width: Int, height: Int): State[Window, Stream[String]] = for
    _ <- setSize(width, height)
    _ <- addBallView()
    _ <- show()
    events <- eventStream()
  yield events

  import scala.concurrent.duration.DurationInt

  val period = 25.millis
  val width = 1000
  val height = 1000

  def updateModel(): State[Game, BallPosition] = seq(updateState(), getBall)

  def updateView(ball: BallPosition): State[Window, Unit] = drawBall(ball._1, ball._2, ball._3)

  val controller = for
    events <- mv(nop(), _ => windowCreation(width, height))
    _ <- gameLoop(events, updateModel(), updateView, period)
  yield ()

  controller((initialState(width, height, ballRadius = 50, maxSpeed = 20), initialWindow))


