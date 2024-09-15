package u06.verifier

object ReadersAndWriters extends App:
  import u06.verifier.util.PetriNets.*
  import u06.verifier.util.PetriNets.Place7.*
  
  // example usage
  println(
    ((1 to 60).to(LazyList) flatMap (readersAndWriters.toSystem.paths(>(P1, P5), _)))
      .flatMap(_.toList)
      .map(marking => marking.asMap)
      .exists(tokensIn => tokensIn.getOrElse(P6, 0) > 0 && tokensIn.getOrElse(P7, 0) > 0)
  )
