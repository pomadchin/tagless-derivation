package cats.tagless

import cats.data.Tuple2K

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of SemigroupalK for ${Alg}")
trait SemigroupalK[Alg[_[_]]] extends Serializable {
  def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[Tuple2K[F, G, *]]
}
