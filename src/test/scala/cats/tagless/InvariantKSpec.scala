package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.Derive
import cats.arrow.FunctionK
import cats.data.Tuple2K
import scala.util.Try
import cats.~>

import scala.compiletime.testing.*

class InvariantKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("InvariantK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val invariantK = Derive.invariantK[SimpleService]
      invariantK `shouldBe` a[InvariantK[SimpleService]]
    }

    it("InvariantK should be a valid instance for a simple algebra") {
      val invariantK = Derive.invariantK[SimpleService]
      val functorK   = Derive.functorK[SimpleService]

      val fk: Id ~> Option  = FunctionK.lift([X] => (id: Id[X]) => Option(id))
      val gk: Option ~> Id  = FunctionK.lift([X] => (id: Option[X]) => id.get)
      val invariantInstance = invariantK.imapK(instance)(fk)(gk)
      val optionalInstance  = functorK.mapK(instance)(fk)

      invariantInstance.id() `shouldBe` optionalInstance.id()
      invariantInstance.list(0) `shouldBe` optionalInstance.list(0)
      invariantInstance.lists(0, 1) `shouldBe` optionalInstance.lists(0, 1)
      invariantInstance.paranthesless `shouldBe` optionalInstance.paranthesless
      invariantInstance.tuple `shouldBe` optionalInstance.tuple
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.invariantK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }
  }
