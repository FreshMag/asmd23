package u07.simulation.mvc

import u04.monads.States.State
import u07.simulation.facade.SwingFunctionalFacade.{Frame, createFrame}

object SPNView:
  trait WindowState:
    type Window

    def initialWindow: Window
    def setSize(width: Int, height: Int): State[Window, Unit]
    def addChartView(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): State[Window, Unit]
    def addChartValue(x: Double, y: Double, rowKey: String): State[Window, Unit]
    def addChartValues(values: Map[String, Double], x: Double): State[Window, Unit]
    def addButton(text: String, name: String): State[Window, Unit]
    def show(): State[Window, Unit]
    def nop(): State[Window, Unit]

    def eventStream(): State[Window, LazyList[String]]

  object WindowStateImpl extends WindowState:

    type Window = Frame

    def initialWindow: Window = createFrame()

    def setSize(width: Int, height: Int): State[Window, Unit] =
      State(w => (w.setSize(width, height), {}))

    def addChartView(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): State[Window, Unit] =
      State(w => (w.createChart(title, xLabel, yLabel, rowLabels), ()))

    override def addChartValue(x: Double, y: Double, rowKey: String): State[Frame, Unit] =
      State(w => (w.addChartValue(x, y, rowKey), ()))

    override def addChartValues(values: Map[String, Double], x: Double): State[Frame, Unit] =
      State(w => (w.addChartValues(values, x), ()))

    override def addButton(text: String, name: String): State[Frame, Unit] =
      State(w => (w.addButton(text, name), ()))

    def show(): State[Window, Unit] =
      State(w => (w.show(), ()))

    def eventStream(): State[Window, LazyList[String]] =
      State(w => (w, LazyList.continually(w.events()())))

    override def nop(): State[Frame, Unit] = State(w => (w, ()))
