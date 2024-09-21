package u07.simulation

import u04.monads.States.State
import u07.simulation.SwingFunctionalFacade.{Frame, createFrame}

import javax.swing.JPanel

object SPNView:
  trait WindowState:
    type Window

    def initialWindow: Window
    def setSize(width: Int, height: Int): State[Window, Unit]
    def addChartView(): State[Window, Unit]
    def show(): State[Window, Unit]
    def nop(): State[Window, Unit]

    def eventStream(): State[Window, LazyList[String]]

  object WindowStateImpl extends WindowState:
    
    type Window = Frame

    def initialWindow: Window = createFrame()

    def setSize(width: Int, height: Int): State[Window, Unit] =
      State(w => (w.setSize(width, height), {}))

    def addChartView(): State[Window, Unit] =
      State(w => (w.addPanel(new JPanel(), "ball-view"), ()))
    
    def show(): State[Window, Unit] =
      State(w => (w.show(), ()))

    def eventStream(): State[Window, LazyList[String]] =
      State(w => (w, LazyList.continually(w.events()())))

    override def nop(): State[Frame, Unit] = State(w => (w, ()))
      
      
