package cats.tagless

import cats.Id
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.tagless.macros.Derive
import cats.arrow.FunctionK
import cats.data.Tuple2K
import scala.util.Try
import cats.~>

class InvariantKSpec extends AnyFunSpec with Matchers with Fixtures:
  describe("InvariantK Spec") {
    trait SimpleService[F[_]]:
      def id(): F[Int]

    // new InvariantK[SimpleService] {
    //   def imapK[F[_], G[_]](af: SimpleService[F])(fk: F ~> G)(gk: G ~> F): SimpleService[G] = new SimpleService[G] {
    //     def id(): G[Int] = ??? // tagless.this.`package`.catsTaglessApplyKForIdK[Int].imapK[F, G](af.id())(fk)(gk)
    //   }
    // }
  }
