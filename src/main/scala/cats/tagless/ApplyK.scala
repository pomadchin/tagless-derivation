package cats.tagless

import cats.~>
import cats.data.Tuple2K
import cats.tagless.macros.Derive

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of ApplyK for ${Alg}")
trait ApplyK[Alg[_[_]]] extends SemigroupalK[Alg] with FunctorK[Alg] {
  def map2K[F[_], G[_], H[_]](af: Alg[F], ag: Alg[G])(f: Tuple2K[F, G, *] ~> H): Alg[H] =
    mapK(productK(af, ag))(f)

  extension [Alg[_[_]], F[_]](inline af: Alg[F])
    inline def map2K[G[_], H[_]](inline ag: Alg[G])(inline f: Tuple2K[F, G, *] ~> H)(using applyK: ApplyK[Alg]): Alg[H] =
      applyK.map2K(af, ag)(f)
}

object ApplyK:
  implicit def catsTaglessApplyKForIdK[A]: ApplyK[IdK[A]#λ] =
    idKInstance.asInstanceOf[ApplyK[IdK[A]#λ]]

  private val idKInstance: ApplyK[IdK[Any]#λ] = new ApplyK[IdK[Any]#λ]:
    def mapK[F[_], G[_]](af: F[Any])(fk: F ~> G)     = fk(af)
    def productK[F[_], G[_]](af: F[Any], ag: G[Any]) = Tuple2K(af, ag)

  inline def derived[Alg[_[_]]] = Derive.applyK[Alg]
