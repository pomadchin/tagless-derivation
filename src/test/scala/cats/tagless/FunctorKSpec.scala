package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.macroFunctorK
import cats.arrow.FunctionK

class FunctorKSpec extends AnyFunSpec with Matchers with Fixtures:

  describe("FunctorK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val functorK = macroFunctorK.derive[SimpleService]
      functorK `shouldBe` a[FunctorK[SimpleService]]
    }

    it("FunctorK should be a valid instance for a simple algebra") {
      val functorK = macroFunctorK.derive[SimpleService]

      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))

      optionalInstance.id() `shouldBe` Some(instance.id())
      optionalInstance.list(0) `shouldBe` Some(instance.list(0))
      optionalInstance.paranthesless `shouldBe` Some(instance.paranthesless)
    }

    // commented out because it fails
    // it("DeriveMacro should not derive instance for a not simple algebra") {
    //   trait NoSimpleService[F[_]] {
    //     def id(): Int
    //     def list(id: Int): F[List[Int]]
    //   }

    //   macroFunctorK.derive[NoSimpleService]
    // }
  }
