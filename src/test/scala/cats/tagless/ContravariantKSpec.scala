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

class ContravariantKSpec extends AnyFunSpec with Matchers:
  import Fixtures.NotSimpleService

  trait SimpleService[F[_]]:
    def id(id: F[Int]): Int
    def ids(id1: F[Int], id2: F[Int]): Int
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[F, String, Int]

  val instance = new SimpleService[Id]:
    def id(id: Id[Int]): Int                 = id
    def ids(id1: Id[Int], id2: Id[Int]): Int = id1 + id2
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[Id, String, Int] =
      Cokleisli.apply((str: Id[String]) => f(init.toInt, str))

  describe("ContravariantK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      def contravariantK = Derive.contravariantK[SimpleService]
      contravariantK `shouldBe` a[ContravariantK[SimpleService]]
    }

    it("DeriveMacro should derive instance for a simple algebra #2") {
      def contravariantK   = Derive.contravariantK[SimpleService]
      val fk: Option ~> Id = FunctionK.lift[Option, Id]([X] => (id: Option[X]) => id.get)
      val optionalInstance = contravariantK.contramapK(instance)(fk)

      val f: (Int, String) => Int = { (i, s) => i + s.toInt }

      optionalInstance.id(Some(23)) `shouldBe` instance.id(23)
      optionalInstance.ids(Some(0), Some(1)) `shouldBe` instance.ids(0, 1)
      optionalInstance.foldSpecialized("0")(f).run(Some("1")) `shouldBe` instance.foldSpecialized("0")(f).run("1")
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.contravariantK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }

    it("ContravariantK derives syntax") {
      trait SimpleServiceSyntax[F[_]] derives ContravariantK:
        def id(i: F[Int]): Int
    }
  }
