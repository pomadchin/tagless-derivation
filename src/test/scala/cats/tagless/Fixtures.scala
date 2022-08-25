package cats.tagless

import cats.Id

trait Fixtures:
  /** Simple algebra definition */
  trait SimpleService[F[_]]:
    def id(): F[Int]
    def list(id: Int): F[List[Int]]
    def lists(id1: Int, id2: Int): F[List[Int]]
    def paranthesless: F[Int]
    def tuple: F[(Int, Long)]

  def instance = new SimpleService[Id]:
    def id(): Id[Int]                            = 42
    def list(id: Int): Id[List[Int]]             = List(id)
    def lists(id1: Int, id2: Int): Id[List[Int]] = List(id1, id2)
    def paranthesless: Id[Int]                   = 23
    def tuple: Id[(Int, Long)]                   = (42, 23)

  trait NotSimpleService[F[_]]:
    def id(): Int
    def list(id: Int): F[List[Int]]

object Fixtures extends Fixtures
