package u06.verifier

import u06.modelling.PetriNet.Marking

object PNChecking:

  def hasTokens[T](marking: Marking[T]): Marking[T] => Boolean =
    m => (marking diff m).size == 0
