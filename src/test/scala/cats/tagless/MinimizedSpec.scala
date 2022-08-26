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

class MinimizedSpec extends AnyFunSpec with Matchers:

  trait SimpleService[F[_]]:
    def id(i: F[Int]): Int
    // def ids(id1: F[Int], id2: F[Int]): Int
    // def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[F, String, Int]

  describe("Minimized Spec") {
    it("DeriveMacro should derive instance for a simple algebra") {
      macroMinimized.derive[SimpleService, Option]
      macroMinimized.derive[SimpleService, Id]
    }
  }
