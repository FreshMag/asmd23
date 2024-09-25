package u07.simulation.utils

object SPNUtils:
  export u04.mvc.MonadicMVC.*

  /**
   * Utility method to convert a map from [[T]] to [[Int]] to a map from [[String]] to [[Double]], filtering with
   * a Set of [[T]]
   * @param eventStateMap map to convert
   * @param filterPlaces [[T]] to filter
   * @tparam T type used inside a [[SPN]]
   * @return the converted [[Map]]
   */
  def eventStateToChartValues[T](eventStateMap: Map[T, Int], filterPlaces: Set[T]): Map[String, Double] =
    eventStateMap
      .filter((place, _) => !filterPlaces.contains(place))
      .map { case (place, value) => (place.toString, value.toDouble) }
