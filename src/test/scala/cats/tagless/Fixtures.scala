package cats.tagless

import cats.Id

trait Fixtures:
  /** Simple algebra definition */
  trait SimpleService[F[_]]:
    def id(): F[Int]
    def list(id: Int): F[List[Int]]
    def paranthesless: F[Int]
    def tuples: F[(Int, Long)]

  def instance = new SimpleService[Id]:
    def id(): Id[Int]                = 42
    def list(id: Int): Id[List[Int]] = List(id)
    def paranthesless: Id[Int]       = 23
    def tuples: Id[(Int, Long)]      = (42, 23)

  trait NoSimpleService[F[_]]:
    def id(): Int
    def list(id: Int): F[List[Int]]
