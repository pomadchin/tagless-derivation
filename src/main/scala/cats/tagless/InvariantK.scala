package cats.tagless

import cats.~>
import cats.tagless.macros.Derive

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of InvariantK for ${Alg}")
trait InvariantK[Alg[_[_]]] extends Serializable {
  def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G]
}

object InvariantK:
  inline def derived[Alg[_[_]]] = Derive.invariantK[Alg]
