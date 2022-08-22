package cats.tagless

import cats.~>

import scala.annotation.implicitNotFound
import cats.tagless.macros.DeriveMacros

/**
 * Sort of a higher kinded Functor, but, well, it's complicated. See Daniel Spiewak's comment here
 * https://github.com/typelevel/cats/issues/2697#issuecomment-453883055 Also explains why this isn't in `cats-core`.
 */
@implicitNotFound("Could not find an instance of FunctorK for ${Alg}")
trait FunctorK[Alg[_[_]]] extends InvariantK[Alg] {
  def mapK[F[_], G[_]](af: Alg[F])(fk: F ~> G): Alg[G]
  def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] = mapK(af)(fk)
}

object Test {

  // var kk: FunctionK[Option] = ???
  // k.apply

  trait UserService[F[_]] {
    def foo(): F[Unit]
  }

  val fkTest = new FunctorK[UserService] {
    def mapK[F[_], G[_]](af: UserService[F])(fk: F ~> G): UserService[G] = new UserService[G] {
      def foo(): G[Unit] = fk.apply[Unit](af.foo())
    }
  }
}
