package cats.tagless

import cats.data.Tuple2K
import cats.tagless.macros.Derive

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of SemigroupalK for ${Alg}")
trait SemigroupalK[Alg[_[_]]] extends Serializable {
  def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[Tuple2K[F, G, *]]

  extension [Alg[_[_]], F[_]](inline af: Alg[F])
    inline def productK[G[_]](inline ag: Alg[G])(using semigroupalK: SemigroupalK[Alg]): Alg[Tuple2K[F, G, *]] =
      semigroupalK.productK(af, ag)
}

object SemigroupalK:
  inline def derived[Alg[_[_]]] = Derive.semigroupalK[Alg]
