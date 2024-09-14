package u04.mvc.engineer

import u04.monads.States.State

import scala.util.Random

object BallModel:

  trait GameState:
    type Ball
    type World
    type Game = (World, Ball)
    type BallPosition = (Int, Int, Int)
    def updateState(): State[Game, Game]
    def getBall: State[Game, BallPosition]

    def nop(): State[Game, Unit]

  object GameStateImpl extends GameState:

    opaque type Ball = BallImpl
    opaque type World = (Int, Int)

    def initialState(width: Int, height: Int, speed: Int = 5, ballRadius: Int = 10): Game =
      ((width, height), Ball.randomBall(width, height, speed, ballRadius))

    def updateState(): State[Game, Game] =
      State((world, ball) =>
        val newX = ball.x + ball.vx
        val newY = ball.y + ball.vy

        // Bounce the ball if it hits the edges
        val (newVx, newVy) = (
          if (newX - ball.radius < 0 || newX + ball.radius > world._1) -ball.vx else ball.vx,
          if (newY - ball.radius < 0 || newY + ball.radius > world._2) -ball.vy else ball.vy
        )
        val newBall = ball.copy(x = newX, y = newY, vx = newVx, vy = newVy)
        ((world, newBall), (world, newBall))
      )

    def getBall: State[Game, BallPosition] =
      State((world, ball) => ((world, ball), (ball.x, ball.y, ball.radius)))

    def nop(): State[Game, Unit] = State(game => (game, ()));

    case class BallImpl(x: Int, y: Int, vx: Int, vy: Int, radius: Int = 10)

    private object Ball:
      def randomBall(width: Int, height: Int, speed: Int, ballRadius: Int): Ball =
        BallImpl(
          Random.between(ballRadius, width - ballRadius - 1),
          Random.between(ballRadius, height - ballRadius - 1),
          speed,
          speed,
          ballRadius
        )
