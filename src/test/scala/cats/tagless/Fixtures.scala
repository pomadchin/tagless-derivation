package cats.tagless

import cats.Id
import cats.data.Cokleisli

trait Fixtures:
  /** Simple algebra definition */
  trait SimpleService[F[_]] derives FunctorK, SemigroupalK, ApplyK, InvariantK:
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

  trait SimpleServiceC[F[_]] derives ContravariantK:
    def id(id: F[Int]): Int
    def ids(id1: F[Int], id2: F[Int]): Int
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[F, String, Int]

  val instancec = new SimpleServiceC[Id]:
    def id(id: Id[Int]): Int                 = id
    def ids(id1: Id[Int], id2: Id[Int]): Int = id1 + id2
    def foldSpecialized(init: String)(f: (Int, String) => Int): Cokleisli[Id, String, Int] =
      Cokleisli.apply((str: Id[String]) => f(init.toInt, str))

object Fixtures extends Fixtures
