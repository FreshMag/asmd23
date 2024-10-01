package u09.qworld.utils

import u09.qworld.QMatrix.Facade
import u09.qworld.World.{Position, State, World}
import u09.qworld.utils.ConsoleUtility.*
import u09.qworld.{QMatrix, World}

import scala.concurrent.{Await, Future}

class RLInstance(
  world: World,
  initialPosition: Position,
  isTerminal: PartialFunction[World.State, Boolean],
  gamma: Double,
  alpha: Double,
  epsilon: Double = 0.0,
  v0: Double = 1,
  learningEpisodes: Int = 10000,
  episodeLength: Int = 100,
  showQValues: Boolean = true
) extends App:

  val rl: QMatrix.Facade = Facade(
    initial = (world, initialPosition),
    terminal = isTerminal,
    gamma,
    alpha,
    epsilon,
    v0
  )

  export rl.{qFunction, makeLearningInstance}

  def show(showStrategy: State => String): Unit =
    println(rl.show(world, showStrategy))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.DurationInt

  val q0 = qFunction
  print(s"Starting a learning instance with $learningEpisodes learning episodes")
  private val resultFuture = Future:
    makeLearningInstance().learn(learningEpisodes, episodeLength, q0)

  while !resultFuture.isCompleted
  do
    print(".")
    Thread.sleep(200)
    print(".")
    Thread.sleep(200)
    print(".")
    Thread.sleep(200)
    (0 until 3).foreach(_ => print("\b"))

  println()
  val q1 = Await.result(resultFuture, 5.seconds)
  println(s"${ANSI_GREEN}Completed!${ANSI_RESET}\n")
  if showQValues then
    println(s"${ANSI_YELLOW}----------------- Value function -----------------${ANSI_RESET}\n")
    show(toConsole(q0.vFunction, "%2.2f", 0.0))
  println(s"${ANSI_CYAN}----------------- Best policy -----------------${ANSI_RESET}\n")
  show(toConsole(s => q1.bestPolicy(s).toString, "%7s", ""))

