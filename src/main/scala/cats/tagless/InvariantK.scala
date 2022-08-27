package cats.tagless

import cats.~>
import cats.tagless.macros.Derive

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of InvariantK for ${Alg}")
trait InvariantK[Alg[_[_]]] extends Serializable {
  def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G]

  extension [Alg[_[_]], F[_]](inline af: Alg[F])
    inline def imapK[G[_]](inline fk: F ~> G)(gk: G ~> F)(using invariantK: InvariantK[Alg]): Alg[G] =
      invariantK.imapK(af)(fk)(gk)
}

object InvariantK:
  inline def derived[Alg[_[_]]] = Derive.invariantK[Alg]
