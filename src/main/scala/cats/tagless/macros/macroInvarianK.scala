package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>
import cats.data.Tuple2K

import scala.annotation.experimental
import compiletime.asMatchable

object macroInvariantK:
  import Utils.*

  private val errorFor = "InvariantK"

  inline def derive[Alg[_[_]]] = ${ invariantK[Alg] }

  @experimental def invariantK[Alg[_[_]]: Type](using Quotes): Expr[InvariantK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new InvariantK[Alg] {
        def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] = ???
        // tagless.this.`package`.catsTaglessApplyKForIdK[Int].imapK[F, G](af.id())(fk)(gk)
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res
