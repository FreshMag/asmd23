package u06.verifier

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import u06.verifier.util.PetriNets.*


class PathsSpec extends AnyFlatSpec with Matchers:

  "A simple Petri net" should "provide the right possible paths" in:
    import u06.verifier.util.PetriNets.Place3.*

    val pn1 = PN[Place3] (
      \(P1) ~~> \(P2),
      \(P2) ~~> \(P3),
      \(P3) ~~> \(P1)
    )

    pn1.paths(\(P1), 3) shouldBe Seq(
      List(\(P1), \(P2), \(P3))
    )
    pn1.paths(\(P1), 4) shouldBe Seq(
      List(\(P1), \(P2), \(P3), \(P1))
    )

  "A more complicated Petri net" should "provide the right possible paths" in:
    import u06.verifier.util.PetriNets.Place5.*

    val pn1 = PN[Place5](
      \(P1) ~~> \(P2),
      \(P2) ~~> \(P3, P4),
      \(P4) ~~> \(P5),
      \(P3) ~~> \(P1),
      \(P5) ~~> \(P1)
    )

    pn1.paths(\(P1), 4).toSet shouldBe Set(
      List(\(P1), \(P2), \(P3, P4), \(P3, P5)),
      List(\(P1), \(P2), \(P3, P4), \(P1, P4)),
    )
    pn1.paths(\(P1), 5).toSet shouldBe Set(
      List(\(P1), \(P2), \(P3, P4), \(P1, P4), \(P1, P5)),
      List(\(P1), \(P2), \(P3, P4), \(P1, P4), \(P2, P4)),
      List(\(P1), \(P2), \(P3, P4), \(P3, P5), \(P1, P5)),
      List(\(P1), \(P2), \(P3, P4), \(P3, P5), \(P3, P1)),
    )
    

