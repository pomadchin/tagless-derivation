package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>
import cats.data.Tuple2K

import scala.annotation.experimental
import compiletime.asMatchable

object macroApplyK:
  import Utils.*

  inline def derive[Alg[_[_]]] = ${ applyK[Alg] }

  @experimental def applyK[Alg[_[_]]: Type](using Quotes): Expr[ApplyK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new ApplyK[Alg] {
        def mapK[F[_], G[_]](af: Alg[F])(fk: F ~> G): Alg[G] =
          ${ macroFunctorK.capture('af, 'fk) }
        def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[Tuple2K[F, G, *]] =
          ${ macroSemigroupalK.capture('af, 'ag) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res
