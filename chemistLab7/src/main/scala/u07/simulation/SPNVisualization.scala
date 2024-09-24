package u07.simulation

import org.jfree.data.xy.XYSeriesCollection
import u06.utils.MSet
import u07.simulation.facade.SwingFunctionalFacade

object SPNVisualization:

  import u07.modelling.CTMCSimulation.newSimulationTrace
  import u07.modelling.SPN.{Marking, SPN, toCTMC}
  import u07.simulation.mvc.SPNView.WindowStateImpl.*

  import java.util.Random

  class SPNVisualization[T](
    spn: SPN[T],
    places: Iterable[T],
    initialMarking: Marking[T],
    placesToHide: Set[T] = Set[T](),
    simulationLength: Int = 100,
    title: String = "Simulation",
    xLabel: String = "Time",
    yLabel: String = "Tokens",
    windowWidth: Int = 1024,
    windowHeight: Int = 720
  ) extends App:

    private val trace = toCTMC(spn).newSimulationTrace(initialMarking, new Random).take(simulationLength)
    private val placesToTrack = places.toSet.diff(placesToHide)
    private val dataset = new XYSeriesCollection()
    private val frame = SwingFunctionalFacade.createFrame()
      .setSize(windowWidth, windowHeight)
      .createChartWithDataset(title, xLabel, yLabel, placesToTrack.map(_.toString), dataset)
    trace.foreach: event =>
      frame.addChartValues(
        event.state.asMap
          .filter((place, _) => !placesToHide.contains(place))
          .map({ case (place, value) => (place.toString, value.toDouble) }),
        event.time
      )
    frame.show()
