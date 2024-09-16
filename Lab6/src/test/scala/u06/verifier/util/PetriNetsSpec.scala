package u06.verifier.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import u06.verifier.util.PetriNets.*
import u06.verifier.util.PetriNets.\

class PetriNetsSpec extends AnyFlatSpec with Matchers:
  "The '\\' operator" should "correctly build a Petri net" in:
    import u06.verifier.util.PetriNets.Place3.*

    val pn1 =
      PN[Place3] (
        \(P1) ~~> \(P2),
        \(P2) ~~> \(P3),
        \(P3) ~~> \(P1)
      )

    pn1.next(\(P1)) shouldBe Set(\(P2))
    pn1.next(\(P2)) shouldBe Set(\(P3))
    pn1.next(\(P3)) shouldBe Set(\(P1))

  "Readers and Writers Petri net" should "represent a correct model" in:
    import u06.verifier.util.PetriNets.Place7.*

    readersAndWriters.next(\(P1)) shouldBe Set(\(P2))
    readersAndWriters.next(\(P2)) shouldBe Set(\(P3), \(P4))
    readersAndWriters.next(\(P3, P5)) shouldBe Set(\(P6, P5))
    readersAndWriters.next(\(P4, P5)) shouldBe Set(\(P7))
    readersAndWriters.next(\(P6, P5)) shouldBe Set(\(P1, P5))
    readersAndWriters.next(\(P7)) shouldBe Set(\(P1, P5))




