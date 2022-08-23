package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.DeriveMacros
import cats.arrow.FunctionK

class FunctorKSpec extends AnyFunSpec with Matchers {
  /** Simple algebra definition */
  trait SimpleService[F[_]] {
    def id(): F[Int]
    def list(id: Int): F[List[Int]]
    def paranthesless: F[Int]
  }

  val instance = new SimpleService[Id] {
    def id(): Id[Int] = 42
    def list(id: Int): Id[List[Int]] = List(id)
    def paranthesless: Id[Int] = 23
  }

  describe("FunctorK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val functorK = DeriveMacros.functorK[SimpleService]
      functorK `shouldBe` a [FunctorK[SimpleService]]
    }

    it("FunctorK should be a valid instance for a simple algebra") {
      val functorK = DeriveMacros.functorK[SimpleService]
      
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Some(id)))

      optionalInstance.id() `shouldBe` Some(instance.id())
      optionalInstance.list(0) `shouldBe` Some(instance.list(0))
      optionalInstance.paranthesless `shouldBe` Some(instance.paranthesless)
    }
  }
}
