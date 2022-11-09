package cats.tagless.macros

import cats.tagless.*
import cats.~>
import cats.data.Tuple2K

import quoted.*
import scala.annotation.experimental
import compiletime.asMatchable

object macroInvariantK:
  import Utils.*

  inline def derive[Alg[_[_]]] = ${ invariantK[Alg] }

  @experimental def invariantK[Alg[_[_]]: Type](using Quotes): Expr[InvariantK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new InvariantK[Alg] {
        def imapK[F[_], G[_]](af: Alg[F])(fk: F ~> G)(gk: G ~> F): Alg[G] =
          ${ capture('af, 'fk, 'gk) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res

  @experimental def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](eaf: Expr[Alg[F]], efk: Expr[F ~> G], egk: Expr[G ~> F])(using Quotes): Expr[Alg[G]] =
    import quotes.reflect.*
    val className = "$anon()"
    val parents   = List(TypeTree.of[Object], TypeTree.of[Alg[G]])
    val decls     = memberSymbolsAsSeen[Alg, G]

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map(method => (method, method.tree)).collect { case (method, DefDef(_, _, typedTree, _)) =>
      DefDef(
        method,
        argss =>
          typedTree.tpe.simplified.asMatchable match
            case AppliedType(_, inner) =>
              val aeaf   = methodApply(eaf)(method, argss)
              val mttree = inner.map(tr => TypeTree.of(using tr.asType))

              // https://github.com/lampepfl/dotty/discussions/16305
              // IdK[Int] is encoded as
              // java.lang.Object {
              //   type λ >: [F >: scala.Nothing <: [_$3 >: scala.Nothing <: scala.Any] => scala.Any] => F[scala.Int] <: [F >: scala.Nothing <: [_$3 >: scala.Nothing <: scala.Any] => scala.Any] => F[scala.Int]
              // }
              // i.e. a Refinement type (Object + the type declaration)
              val applyKTypeRepr = TypeRepr.of[IdK].appliedTo(inner) match
                case repr: Refinement => TypeRepr.of[ApplyK].appliedTo(repr.info)
                case repr             => report.errorAndAbort(s"IdK has no proper Refinement type: ${repr}") 

              val instanceK = Implicits.search(applyKTypeRepr) match
                case res: ImplicitSearchSuccess => res.tree
                case _                          => report.errorAndAbort(s"No ${applyKTypeRepr.show} implicit found.") 

              Some(
                Apply(
                  Apply(
                    Select.overloaded(
                      instanceK,
                      "imapK",
                      List(TypeRepr.of[F], TypeRepr.of[G]),
                      List(aeaf)
                    ),
                    List(efk.asTerm)
                  ),
                  List(egk.asTerm)
                )
              )

            case _ => report.errorAndAbort("Derive works with simple algebras only.")
      )
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val expr   = Block(List(clsDef), newCls).asExpr

    // println("============")
    // println(expr.show)
    // println("============")
    expr.asExprOf[Alg[G]]
