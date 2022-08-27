package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.*
import cats.arrow.FunctionK
import cats.data.{Cokleisli, Tuple2K}
import scala.util.Try
import cats.~>

import scala.compiletime.testing.*

class ContravariantKSpec extends AnyFunSpec with Matchers with Fixtures:

  describe("ContravariantK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      def contravariantK = Derive.contravariantK[SimpleServiceC]
      contravariantK `shouldBe` a[ContravariantK[SimpleServiceC]]
    }

    it("DeriveMacro should derive instance for a simple algebra #2") {
      def contravariantK   = Derive.contravariantK[SimpleServiceC]
      val fk: Option ~> Id = FunctionK.lift[Option, Id]([X] => (id: Option[X]) => id.get)
      val optionalInstance = contravariantK.contramapK(instancec)(fk)

      val f: (Int, String) => Int = { (i, s) => i + s.toInt }

      optionalInstance.id(Some(23)) `shouldBe` instancec.id(23)
      optionalInstance.ids(Some(0), Some(1)) `shouldBe` instancec.ids(0, 1)
      optionalInstance.foldSpecialized("0")(f).run(Some("1")) `shouldBe` instancec.foldSpecialized("0")(f).run("1")
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.contravariantK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("ContravariantK derives syntax") {
      val fk: Option ~> Id = FunctionK.lift[Option, Id]([X] => (id: Option[X]) => id.get)
      val optionalInstance = instancec.contramapK(fk)

      val f: (Int, String) => Int = { (i, s) => i + s.toInt }

      optionalInstance.id(Some(23)) `shouldBe` instancec.id(23)
      optionalInstance.ids(Some(0), Some(1)) `shouldBe` instancec.ids(0, 1)
      optionalInstance.foldSpecialized("0")(f).run(Some("1")) `shouldBe` instancec.foldSpecialized("0")(f).run("1")
    }
  }
