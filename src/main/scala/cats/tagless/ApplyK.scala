package cats.tagless

import cats.data.Tuple2K
import cats.~>

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of ApplyK for ${Alg}")
trait ApplyK[Alg[_[_]]] extends SemigroupalK[Alg] with FunctorK[Alg] {
  def map2K[F[_], G[_], H[_]](af: Alg[F], ag: Alg[G])(f: Tuple2K[F, G, *] ~> H): Alg[H] =
    mapK(productK(af, ag))(f)
}
