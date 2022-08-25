package cats.tagless

import cats.~>
import cats.data.Cokleisli

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of InvariantK for ${Alg}")
trait InvariantK[Alg[_[_]]] extends Serializable {
  def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G]
}

object InvariantK:
  implicit def catsTaglessContravariantKForCokleisli[A, B]: ContravariantK[[W[_]] =>> Cokleisli[W, A, B]] =
    cokleisliInstance.asInstanceOf[ContravariantK[[W[_]] =>> Cokleisli[W, A, B]]]

  private val cokleisliInstance: ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] =
    new ContravariantK[[W[_]] =>> Cokleisli[W, Any, Any]] {
      def contramapK[F[_], G[_]](af: Cokleisli[F, Any, Any])(fk: G ~> F) = Cokleisli(ga => af.run(fk(ga)))
    }
