package u04

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.{be, should}

class FailingTest extends AnyFlatSpec with Matchers:
  "test" should "fail" in:
    true shouldBe false


