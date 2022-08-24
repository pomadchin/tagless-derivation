package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.*
import cats.arrow.FunctionK
import cats.data.Tuple2K
import scala.util.Try
import cats.~>

import scala.compiletime.testing.*

class ContravariantKSpec extends AnyFunSpec with Matchers:
  trait SimpleService[F[_]]:
    def id(id: F[Int]): Int
    def ids(id1: F[Int], id2: F[Int]): Int
    def foldSpecialized(init: String)(f: (Int, String) => Int): cats.data.Cokleisli[F, String, Int]

  describe("ContravariantK Spec") {
    macroContravariantK.derive[SimpleService]
  }
