package cats.tagless

import cats.~>

import scala.annotation.implicitNotFound

/**
 * A higher-kinded `Contravariant` functor. Must obey the laws in `cats.tagless.laws.ContravariantKLaws`.
 */
@implicitNotFound("Could not find an instance of ContravariantK for ${Alg}")
trait ContravariantK[Alg[_[_]]] extends InvariantK[Alg] {
  def contramapK[F[_], G[_]](af: Alg[F])(fk: G ~> F): Alg[G]
  override def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] = contramapK(af)(gk)
}
