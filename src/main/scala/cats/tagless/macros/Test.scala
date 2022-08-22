package cats.tagless.macros

trait UserService[F[_]] {
  def id(): F[Int]
  def list(id: Int): F[List[Int]]
  def paranthesless: F[Int]
}

trait SimpleTrait {
  def id: Int
}

object Test {


  // val t = DeriveMacros.functorK[UserService]


  @main() def main() = {
    val res = DeriveMacros.functorKInvoke[UserService]
    //val res = DeriveMacros.functorInvoke[SimpleTrait]
    println(res)
    println(res.getClass())
  }
}

