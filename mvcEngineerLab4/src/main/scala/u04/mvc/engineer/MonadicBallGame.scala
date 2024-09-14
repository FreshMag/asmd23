package u04.mvc.engineer

import u04.monads.Monads.*
import u04.monads.Monads.Monad.*
import u04.monads.States.State
import u04.monads.States.State.*
import u04.mvc.engineer.BallController.ControllerImpl.*
import u04.mvc.engineer.BallModel.GameStateImpl.*
import u04.mvc.engineer.BallView.WindowStateImpl.*

import scala.collection.immutable.LazyList

/**
 * Simple ball bouncing on walls simulation.
 */
@main def ballGame(): Unit =

  def windowCreation(width: Int, height: Int): State[Window, LazyList[String]] =
    for
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

  val controller =
    for
      events <- mv(nop(), _ => windowCreation(width, height))
      _ <- gameLoop(events, updateModel(), updateView, period)
    yield ()

  controller((initialState(width, height, ballRadius = 50, speed = 20), initialWindow))
