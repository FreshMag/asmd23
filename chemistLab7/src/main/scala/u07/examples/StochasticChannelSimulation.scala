package u07.examples

import u07.examples.StochasticChannel.*
import u07.utils.Time

import java.util.Random

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      stocChannel.newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")