package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>
import cats.data.Tuple2K

import scala.annotation.experimental
import compiletime.asMatchable

object macroSemigroupalK {
  import Utils.*

  inline def derive[Alg[_[_]]] = ${ semigroupalK[Alg] }

  @experimental def semigroupalK[Alg[_[_]]: Type](using Quotes): Expr[SemigroupalK[Alg]] = {
    import quotes.reflect.*

    val res = '{
      new SemigroupalK[Alg] {
        def productK[F[_], G[_]](af: Alg[F], ag: Alg[G]): Alg[Tuple2K[F, G, *]] =
          ${ capture('af, 'ag) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res
  }

  @experimental
  def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](e1: Expr[Alg[F]], e2: Expr[Alg[G]])(using Quotes): Expr[Alg[Tuple2K[F, G, *]]] =
    import quotes.reflect.*
    val className = "$anon()"
    val parents   = List(TypeTree.of[Object], TypeTree.of[Alg[Tuple2K[F, G, *]]])
    val decls     = definedMethodsInTypeWithOwner[Alg]

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map(method => (method, method.tree)).collect { case (method, DefDef(_, _, typedTree, _)) =>
      DefDef(
        method,
        argss =>
          typedTree.tpe.simplified.asMatchable match
            case at @ AppliedType(tr, inner) =>
              val ae1 =
                // method with no parentheses
                if argss.isEmpty then Select(e1.asTerm, method)
                else Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

              val ae2 =
                // method with no parentheses
                if argss.isEmpty then Select(e2.asTerm, method)
                else Apply(Select(e2.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

              // println("---------")
              // println(inner)
              // println("---------")

              val innerTree: List[TypeTree] = inner.map(tr => TypeTree.of(using tr.asType))

              Some(
                Apply(
                  TypeApply(Ref(Symbol.requiredMethod("cats.data.Tuple2K.apply")), innerTree),
                  List(ae1, ae2)
                )
              )
            case _ => report.errorAndAbort("SemigroupalK can be derived for simple algebras only.")
      )
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[Tuple2K[F, G, *]]])
    val expr   = Block(List(clsDef), newCls).asExpr

    // println("============")
    // println(expr.show)
    // println("============")
    expr.asExprOf[Alg[Tuple2K[F, G, *]]]
}
