package cats.tagless

import cats.~>
import cats.data.Cokleisli
import cats.tagless.macros.Derive

import scala.annotation.implicitNotFound

/**
 * A higher-kinded `Contravariant` functor. Must obey the laws in `cats.tagless.laws.ContravariantKLaws`.
 */
@implicitNotFound("Could not find an instance of ContravariantK for ${Alg}")
trait ContravariantK[Alg[_[_]]] extends InvariantK[Alg] {
  def contramapK[F[_], G[_]](af: Alg[F])(fk: G ~> F): Alg[G]
  override def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] = contramapK(af)(gk)

  extension [Alg[_[_]], F[_]](inline af: Alg[F])
    inline def contramapK[G[_]](inline fk: G ~> F)(using contravariantK: ContravariantK[Alg]): Alg[G] =
      contravariantK.contramapK(af)(fk)
}

object ContravariantK:
  implicit def catsTaglessContravariantKForCokleisli[A, B]: ContravariantK[[W[_]] =>> Cokleisli[W, A, B]] =
    cokleisliInstance.asInstanceOf[ContravariantK[[W[_]] =>> Cokleisli[W, A, B]]]

  private val cokleisliInstance: ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] =
    new ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] {
      def contramapK[F[_], G[_]](af: Cokleisli[F, Any, Any])(fk: G ~> F) = Cokleisli(ga => af.run(fk(ga)))
    }

  inline def derived[Alg[_[_]]] = Derive.contravariantK[Alg]
