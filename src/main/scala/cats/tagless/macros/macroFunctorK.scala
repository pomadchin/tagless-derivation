package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>

import scala.annotation.experimental
import compiletime.asMatchable

object macroFunctorK:
  import Utils.*

  private val errorFor = "FunctorK"

  inline def derive[Alg[_[_]]] = ${ functorK[Alg] }

  @experimental def functorK[Alg[_[_]]: Type](using Quotes): Expr[FunctorK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new FunctorK[Alg] {
        def mapK[F[_], G[_]](af: Alg[F])(fk: F ~> G): Alg[G] =
          ${ capture('af, 'fk)(errorFor) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res

  @experimental def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](eaf: Expr[Alg[F]], efk: Expr[F ~> G])(errFor: String)(using Quotes): Expr[Alg[G]] =
    import quotes.reflect.*
    val className = "$anon()"
    val parents   = List(TypeTree.of[Object], TypeTree.of[Alg[G]])
    val decls     = definedMethodsInTypeWithOwner[Alg]

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map(method => (method, method.tree)).collect { case (method, DefDef(_, _, typedTree, _)) =>
      DefDef(
        method,
        argss =>
          typedTree.tpe.simplified.asMatchable match
            case AppliedType(_, inner) =>
              val apply = methodApply(eaf)(method, argss)
              Some(Select.overloaded(efk.asTerm, "apply", inner, List(apply)))
            case _ => report.errorAndAbort(s"$errFor can be derived for simple algebras only.")
      )
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val expr   = Block(List(clsDef), newCls).asExpr

    // println("============")
    // println(expr.show)
    // println("============")
    expr.asExprOf[Alg[G]]
