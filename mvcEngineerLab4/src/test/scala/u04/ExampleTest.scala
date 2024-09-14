package u04

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExampleTest extends AnyFlatSpec with Matchers:

  "An example test" should "pass" in: 
    true shouldBe true
