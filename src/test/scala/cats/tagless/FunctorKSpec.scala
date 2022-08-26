package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.Derive
import cats.arrow.FunctionK

import scala.compiletime.testing.*

class FunctorKSpec extends AnyFunSpec with Matchers with Fixtures:

  describe("FunctorK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val functorK = Derive.functorK[SimpleService]
      functorK `shouldBe` a[FunctorK[SimpleService]]
    }

    it("FunctorK should be a valid instance for a simple algebra") {
      val functorK = Derive.functorK[SimpleService]

      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))

      optionalInstance.id() `shouldBe` Some(instance.id())
      optionalInstance.list(0) `shouldBe` Some(instance.list(0))
      optionalInstance.lists(0, 1) `shouldBe` Some(instance.lists(0, 1))
      optionalInstance.paranthesless `shouldBe` Some(instance.paranthesless)
      optionalInstance.tuple `shouldBe` Some(instance.tuple)
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.functorK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("FunctorK derives syntax") {
      trait SimpleServiceSyntax[F[_]] derives FunctorK:
        def id(): F[Int]
    }
  }
