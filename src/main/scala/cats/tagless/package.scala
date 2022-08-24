package cats

package object tagless:
  type IdK[A] = { type λ[F[_]] = F[A] }
