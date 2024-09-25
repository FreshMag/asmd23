package u07.simulation.mvc

import u04.monads.States.State
import u07.simulation.facade.SwingFunctionalFacade.{Frame, createFrame}

object SPNView:

  /**
   * Trait representing the state and actions available for a window in the simulation.
   */
  trait WindowState:
    /**
     * Abstract type representing the window component in the UI.
     */
    type Window

    /**
     * Creates the initial window.
     *
     * @return
     *   The initial window.
     */
    def initialWindow: Window

    /**
     * Sets the size of the window.
     *
     * @param width
     *   The width of the window.
     * @param height
     *   The height of the window.
     * @return
     *   A state transition function that sets the window size.
     */
    def setSize(width: Int, height: Int): State[Window, Unit]

    /**
     * Adds a chart view to the window with specified labels.
     *
     * @param title
     *   The title of the chart.
     * @param xLabel
     *   The label for the x-axis.
     * @param yLabel
     *   The label for the y-axis.
     * @param rowLabels
     *   The row labels for the chart.
     * @return
     *   A state transition function that adds a chart view to the window.
     */
    def addChartView(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): State[Window, Unit]

    /**
     * Adds a single chart value to the chart view in the window.
     *
     * @param x
     *   The x-coordinate (time) for the chart.
     * @param y
     *   The y-coordinate (value) for the chart.
     * @param rowKey
     *   The row key to which the value belongs.
     * @return
     *   A state transition function that adds the chart value.
     */
    def addChartValue(x: Double, y: Double, rowKey: String): State[Window, Unit]

    /**
     * Adds multiple chart values at once, for various rows in the chart.
     *
     * @param values
     *   A map of row keys to values to be added.
     * @param x
     *   The x-coordinate (time) for the values.
     * @return
     *   A state transition function that adds multiple values to the chart.
     */
    def addChartValues(values: Map[String, Double], x: Double): State[Window, Unit]

    /**
     * Adds a button to the window with the specified text and action name.
     *
     * @param text
     *   The button label.
     * @param name
     *   The action command name for the button.
     * @return
     *   A state transition function that adds the button to the window.
     */
    def addButton(text: String, name: String): State[Window, Unit]

    /**
     * Displays the window to the user.
     *
     * @return
     *   A state transition function that makes the window visible.
     */
    def show(): State[Window, Unit]

    /**
     * Returns a stream of events triggered by user actions in the window.
     *
     * @return
     *   A state transition function that returns an event stream as a LazyList of event names.
     */
    def eventStream(): State[Window, LazyList[String]]

    /**
     * Represents a no-operation (nop) action in the window.
     *
     * @return
     *   A state transition function that leaves the window unchanged.
     */
    def nop(): State[Window, Unit]

  /**
   * Implementation of the WindowState trait that uses the Frame component from the SwingFunctionalFacade.
   */
  object WindowStateImpl extends WindowState:

    // The type representing the window is a Frame.
    type Window = Frame

    /**
     * Creates the initial window using the SwingFunctionalFacade's `createFrame` method.
     *
     * @return
     *   A new window (Frame).
     */
    def initialWindow: Window = createFrame()

    /**
     * Sets the size of the window (Frame).
     *
     * @param width
     *   The width of the window.
     * @param height
     *   The height of the window.
     * @return
     *   A state transition function that sets the window size.
     */
    def setSize(width: Int, height: Int): State[Window, Unit] =
      State(w => (w.setSize(width, height), {}))

    /**
     * Adds a chart view to the window (Frame) with the given title, labels, and rows.
     *
     * @param title
     *   The chart title.
     * @param xLabel
     *   The label for the x-axis.
     * @param yLabel
     *   The label for the y-axis.
     * @param rowLabels
     *   The row labels for the chart.
     * @return
     *   A state transition function that adds a chart to the window.
     */
    def addChartView(title: String, xLabel: String, yLabel: String, rowLabels: Iterable[String]): State[Window, Unit] =
      State(w => (w.createChart(title, xLabel, yLabel, rowLabels), ()))

    /**
     * Adds a single value to a specific row in the chart view.
     *
     * @param x
     *   The x-coordinate (time) of the value.
     * @param y
     *   The y-coordinate (value) to be added.
     * @param rowKey
     *   The row key representing the series in the chart.
     * @return
     *   A state transition function that adds a chart value.
     */
    override def addChartValue(x: Double, y: Double, rowKey: String): State[Frame, Unit] =
      State(w => (w.addChartValue(x, y, rowKey), ()))

    /**
     * Adds multiple values to different rows of the chart view.
     *
     * @param values
     *   A map of row keys to y-values to be added.
     * @param x
     *   The x-coordinate (time) of the values.
     * @return
     *   A state transition function that adds multiple chart values.
     */
    override def addChartValues(values: Map[String, Double], x: Double): State[Frame, Unit] =
      State(w => (w.addChartValues(values, x), ()))

    /**
     * Adds a button to the window with the specified text and action command.
     *
     * @param text
     *   The button label.
     * @param name
     *   The action command name for the button.
     * @return
     *   A state transition function that adds the button to the window.
     */
    override def addButton(text: String, name: String): State[Frame, Unit] =
      State(w => (w.addButton(text, name), ()))

    /**
     * Displays the window to the user.
     *
     * @return
     *   A state transition function that makes the window visible.
     */
    def show(): State[Window, Unit] =
      State(w => (w.show(), ()))

    /**
     * Provides a LazyList of user-triggered events from the window.
     *
     * @return
     *   A state transition function that returns a stream of events.
     */
    def eventStream(): State[Window, LazyList[String]] =
      State(w => (w, LazyList.continually(w.events()())))

    /**
     * Represents a no-operation (nop) action for the window.
     *
     * @return
     *   A state transition function that leaves the window unchanged.
     */
    override def nop(): State[Frame, Unit] = State(w => (w, ()))
