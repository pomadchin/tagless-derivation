package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.*
import cats.arrow.FunctionK
import cats.data.Tuple2K
import scala.util.Try
import cats.~>

class ApplyKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("ApplyK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val applyK = macroApplyK.derive[SimpleService]
      applyK `shouldBe` a[ApplyK[SimpleService]]
    }

    it("ApplyK should be a valid instance for a simple algebra") {
      val functorK         = macroFunctorK.derive[SimpleService]
      val semigroupalK     = macroSemigroupalK.derive[SimpleService]
      val applyK           = macroApplyK.derive[SimpleService]
      val optionalInstance = functorK.mapK(instance)(FunctionK.lift([X] => (id: Id[X]) => Option(id)))
      val combinedInstance = semigroupalK.productK(instance, optionalInstance)

      val fk: Tuple2K[Id, Option, *] ~> Try = FunctionK.lift([X] => (tup: Tuple2K[Id, Option, X]) => Try(tup.second.map(_ => tup.first).get))

      val akInstance = applyK.map2K[Id, Option, Try](instance, optionalInstance)(fk)

      akInstance.id() `shouldBe` Try(instance.id())
      akInstance.list(0) `shouldBe` Try(instance.list(0))
      akInstance.paranthesless `shouldBe` Try(instance.paranthesless)
      akInstance.tuples `shouldBe` Try(instance.tuples)
    }
  }
