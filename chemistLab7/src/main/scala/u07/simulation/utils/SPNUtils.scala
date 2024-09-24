package u07.simulation.utils

object SPNUtils:
  export u04.mvc.MonadicMVC.*

  def eventStateToChartValues[T](eventStateMap: Map[T, Int], filterPlaces: Set[T]): Map[String, Double] =
    eventStateMap
      .filter((place, _) => !filterPlaces.contains(place))
      .map { case (place, value) => (place.toString, value.toDouble) }
