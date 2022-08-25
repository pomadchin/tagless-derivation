package cats.tagless

import cats.~>
import cats.data.Cokleisli

import scala.annotation.implicitNotFound

/**
 * A higher-kinded `Contravariant` functor. Must obey the laws in `cats.tagless.laws.ContravariantKLaws`.
 */
@implicitNotFound("Could not find an instance of ContravariantK for ${Alg}")
trait ContravariantK[Alg[_[_]]] extends InvariantK[Alg] {
  def contramapK[F[_], G[_]](af: Alg[F])(fk: G ~> F): Alg[G]
  override def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] = contramapK(af)(gk)
}

object ContravariantK:
  implicit def catsTaglessContravariantKForCokleisli[A, B]: ContravariantK[[W[_]] =>> Cokleisli[W, A, B]] =
    cokleisliInstance.asInstanceOf[ContravariantK[[W[_]] =>> Cokleisli[W, A, B]]]

  private val cokleisliInstance: ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] =
    new ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] {
      def contramapK[F[_], G[_]](af: Cokleisli[F, Any, Any])(fk: G ~> F) = Cokleisli(ga => af.run(fk(ga)))
    }
