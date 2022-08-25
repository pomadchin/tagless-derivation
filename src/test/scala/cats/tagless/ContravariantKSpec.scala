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

  // val instance = new SimpleService[Id]:
  //   def id(id: Id[Int]): Int = 2
  //   def ids(id1: Id[Int], id2: Id[Int]): Int = id1 + id2
  //   def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[Id, String, Int] =
  //     Cokleisli(str => f.curried(init.toInt)(str))

  describe("ContravariantK Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      val contravariantK = Derive.contravariantK[SimpleService]
      contravariantK `shouldBe` a[ContravariantK[SimpleService]]
    }

    it("ContravariantK should be a valid instance for a simple algebra") {
      // val contravariantK = Derive.contravariantK[SimpleService]
      // contravariantK.contramapK(instance)
    }

    it("DeriveMacro should not derive instance for a not simple algebra") {
      typeCheckErrors("Derive.contravariantK[NotSimpleService]").map(_.message) `shouldBe` List("Derive works with simple algebras only.")
    }
  }
