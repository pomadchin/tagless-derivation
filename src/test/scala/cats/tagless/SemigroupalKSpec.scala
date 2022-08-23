package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.*
import cats.arrow.FunctionK
import cats.data.Tuple2K

class SemigroupalKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("SemigorupalK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val semigroupalK = macroSemigroupalK.derive[SimpleService]
      semigroupalK `shouldBe` a[SemigroupalK[SimpleService]]
    }

    it("SemigorupalK should be a valid instance for a simple algebra") {
      val functorK         = macroFunctorK.derive[SimpleService]
      val semigroupalK     = macroSemigroupalK.derive[SimpleService]
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))
      val combinedInstance = semigroupalK.productK(instance, optionalInstance)

      combinedInstance.id() `shouldBe` Tuple2K(instance.id(), optionalInstance.id())
      combinedInstance.list(0) `shouldBe` Tuple2K(instance.list(0), optionalInstance.list(0))
      combinedInstance.paranthesless `shouldBe` Tuple2K(instance.paranthesless, optionalInstance.paranthesless)
    }
  }
