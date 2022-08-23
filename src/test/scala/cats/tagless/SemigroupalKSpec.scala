package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.Derive
import cats.arrow.FunctionK
import cats.data.Tuple2K

class SemigroupalKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("SemigorupalK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val semigroupalK = Derive.semigroupalK[SimpleService]
      semigroupalK `shouldBe` a[SemigroupalK[SimpleService]]
    }

    it("SemigorupalK should be a valid instance for a simple algebra") {
      val functorK         = Derive.functorK[SimpleService]
      val semigroupalK     = Derive.semigroupalK[SimpleService]
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))
      val combinedInstance = semigroupalK.productK(instance, optionalInstance)

      combinedInstance.id() `shouldBe` Tuple2K(instance.id(), optionalInstance.id())
      combinedInstance.list(0) `shouldBe` Tuple2K(instance.list(0), optionalInstance.list(0))
      combinedInstance.paranthesless `shouldBe` Tuple2K(instance.paranthesless, optionalInstance.paranthesless)
      combinedInstance.tuple `shouldBe` Tuple2K(instance.tuple, optionalInstance.tuple)
    }

    // commented out because it fails to compile, q: how to cover it with tests?
    // it("DeriveMacro should not derive instance for a not simple algebra") {
    //   Derive.semigroupalK[NotSimpleService]
    // }
  }
