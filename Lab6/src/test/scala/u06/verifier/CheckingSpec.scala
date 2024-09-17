package u06.verifier

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import u06.verifier.util.PetriNets.*
import u06.verifier.Checking.*
import u06.verifier.PNChecking.*


class CheckingSpec extends AnyFlatSpec with Matchers:
  "Trivial uses of 'satisfies'" should "work as expected" in :
    import u06.verifier.util.PetriNets.Place3ME.*

    mutualExclusion satisfies true shouldBe true
    mutualExclusion satisfies {
      system.next(\(N)) == Set(\(T))
    } shouldBe true

    mutualExclusion satisfies {
      system.normalForm(system.next(\(C)).head)
    } shouldBe true

    mutualExclusion satisfies {
      system.complete(List(\(N, N), \(T, N), \(C, N), \(N)))
    } shouldBe false

    mutualExclusion satisfies {
      system.complete(List(\(N, N), \(T, N), \(C, N), \(N), \(T), \(C), \()))
    } shouldBe true

  "The 'E' method inside 'satisfies'" should "return true only if a certain element is present in the system" in:
    import u06.verifier.util.PetriNets.Place3ME.*
    mutualExclusion satisfies {
      E(next(\(N, N, N)))(_ == \(N, N, T))
    } shouldBe true
    mutualExclusion satisfies {
      notE(next(\(N, N, N)))(_ == \(N, N, T))
    } shouldBe false

  "The 'path' method combined with 'from'" should "return the valid paths" in:
    import u06.verifier.util.PetriNets.Place3ME.*
    given Limit = 5
    val paths = (path from \(N))(mutualExclusion)
    paths.toSet shouldBe Set(
      List(\(N)),
      List(\(N), \(T)),
      List(\(N), \(T), \(C)),
      List(\(N), \(T), \(C), \())
    )

  "The 'E' combined with 'path' and 'suchThat'" should "check for valid paths" in:
    import u06.verifier.util.PetriNets.Place7.*
    given Limit = 5

    val validStartingPoints = List(\(P1), \(P2), \(P3), \(P4))
    val resultFromP1 = for
      start <- validStartingPoints
      if readersAndWriters satisfies:
        E(path from start) suchThat (_.nonEmpty)
    yield true

    resultFromP1.size shouldBe validStartingPoints.size

    val invalidStartingPoints = List(\(P5), \(), \(P5, P5))
    val resultFromP5 = for
      start <- invalidStartingPoints
      if readersAndWriters satisfies:
        E(path from start) suchThat (_.size > 1)
    yield true
    resultFromP5.size shouldBe 0

  "The 'hasMarking' method" should "return true only if a path has a certain marking" in:
    import u06.verifier.util.PetriNets.Place7.*
    (List(\(P1, P2), \(P1, P3), \(P1, P4, P5)) hasMarkingThat hasTokens(\(P5))) shouldBe true
    (List(\(P1), \(P1, P2), \(P1)) hasMarkingThat hasTokens(\(P5))) shouldBe false
    (List(\(P1), \(P1, P2), \(P1, P1, P1)) hasMarkingThat hasTokens(\(P1, P1, P1, P1))) shouldBe false
    (List(\(P1), \(P1, P2), \(P1, P1, P1)) hasMarkingThat hasTokens(\(P1, P1))) shouldBe true
    (List(\(P1, P2), \(P1, P3), \(P1, P4, P5)) hasMarkingThat (_.disjointed(\(P6, P7)))) shouldBe true
    (List(\(P1, P2), \(P1, P3), \(P1, P4, P5)) hasMarkingThat (_.size > 2)) shouldBe true
    (List(\(P1, P2), \(P1, P3), \(P1, P4, P5)) hasMarkingThat (_.size == 0)) shouldBe false
    
  "The 'reachable' method" should "correctly determine whether a certain marking is reachable" in :
    import u06.verifier.util.PetriNets.Place7.*
    import u06.verifier.Checking.given

    given Limit = 50
    readersAndWriters satisfies (reachable(\(P7)) from \(P1)) shouldBe false
    readersAndWriters satisfies (reachable(\(P7)) from \(P1, P5)) shouldBe true
    readersAndWriters satisfies (reachable(\(P5)) from \(P1)) shouldBe false
    readersAndWriters satisfies (reachable(\(P6, P6)) from \(P1, P1, P5)) shouldBe true
    readersAndWriters satisfies (reachable(\(P6, P7)) from \(P1, P5)) shouldBe false // mutual exclusion
