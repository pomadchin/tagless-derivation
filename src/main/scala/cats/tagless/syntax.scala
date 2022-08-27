package cats.tagless

import cats.syntax.functor

import cats.~>
import cats.data.Tuple2K
import cats.syntax.contravariant

object syntax:
  object functorK:
    extension [Alg[_[_]], F[_]](inline af: Alg[F])
      inline def mapK[G[_]](inline fk: F ~> G)(using functorK: FunctorK[Alg]): Alg[G] =
        functorK.mapK(af)(fk)

  object applyK:
    extension [Alg[_[_]], F[_]](inline af: Alg[F])
      inline def map2K[G[_], H[_]](inline ag: Alg[G])(inline f: Tuple2K[F, G, *] ~> H)(using applyK: ApplyK[Alg]): Alg[H] =
        applyK.map2K(af, ag)(f)

  object semigroupalK:
    extension [Alg[_[_]], F[_]](inline af: Alg[F])
      inline def productK[G[_]](inline ag: Alg[G])(using semigroupalK: SemigroupalK[Alg]): Alg[Tuple2K[F, G, *]] =
        semigroupalK.productK(af, ag)

  object invariantK:
    extension [Alg[_[_]], F[_]](inline af: Alg[F])
      inline def imapK[G[_]](inline fk: F ~> G)(gk: G ~> F)(using invariantK: InvariantK[Alg]): Alg[G] =
        invariantK.imapK(af)(fk)(gk)

  object contravariantK:
    extension [Alg[_[_]], F[_]](inline af: Alg[F])
      inline def contramapK[G[_]](inline fk: G ~> F)(using contravariantK: ContravariantK[Alg]): Alg[G] =
        contravariantK.contramapK(af)(fk)
