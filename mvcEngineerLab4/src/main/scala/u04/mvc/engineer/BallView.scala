package u04.mvc.engineer

import u04.monads.States.State

import scala.collection.immutable.LazyList

object BallView:

  trait WindowState:
    type Window
    def initialWindow: Window
    def setSize(width: Int, height: Int): State[Window, Unit]
    def addBallView(): State[Window, Unit]
    def drawBall(x: Int, y: Int, radius: Int): State[Window, Unit]
    def show(): State[Window, Unit]
    def eventStream(): State[Window, LazyList[String]]

  object WindowStateImpl extends WindowState:

    import u04.mvc.engineer.SwingFunctionalFacade.*

    type Window = Frame

    def initialWindow: Window = createFrame

    def setSize(width: Int, height: Int): State[Window, Unit] =
      State(w => (w.setSize(width, height), {}))

    def addBallView(): State[Window, Unit] =
      State(w => (w.addPanel(SwingCustomComponents.BallPanel(), "ball-view"), ()))

    def drawBall(centerX: Int, centerY: Int, radius: Int): State[Window, Unit] =
      State(w => (w.drawEllipse("ball-view", centerX - radius, centerY - radius, radius), ()))

    def show(): State[Window, Unit] =
      State(w => (w.show(), ()))

    def eventStream(): State[Window, LazyList[String]] =
      State(w => (w, LazyList.continually(w.events().get)))
