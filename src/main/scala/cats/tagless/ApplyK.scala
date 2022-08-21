package cats.tagless

import cats.data.Tuple2K
import cats.~>

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of ApplyK for ${Alg}")
trait ApplyK[Alg[_[_]]] extends SemigroupalK[Alg] with FunctorK[Alg] {
  def map2K[F[_], G[_], H[_]](af: Alg[F], ag: Alg[G])(f: Tuple2K[F, G, *] ~> H): Alg[H] =
    mapK(productK(af, ag))(f)
}

object ApplyK {
  // implicit val serviceApplyK = new ApplyK[Service] {
  //     def productK[F[_], G[_]](af: Service[F], ag: Service[G]): Service[Tuple2K[F, G, *]] = new Service[Tuple2K[F, G, *]] {
  //       def list: Tuple2K[F, G, List[User]] = new Tuple2K[F, G, List[User]](af.list, ag.list)
  //     }

  //     def mapK[F[_], G[_]](af: Service[F])(fk: F ~> G): Service[G] = new Service[G] {
  //       def list: G[List[User]] = fk(af.list)
  //     }
  //   }
}
