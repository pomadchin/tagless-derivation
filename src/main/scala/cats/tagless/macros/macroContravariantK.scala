package cats.tagless.macros

import cats.tagless.*
import cats.~>

import quoted.*
import scala.annotation.experimental
import compiletime.asMatchable

object macroContravariantK:
  import Utils.*

  inline def derive[Alg[_[_]]] = ${ contravariantK[Alg] }

  @experimental def contravariantK[Alg[_[_]]: Type](using Quotes): Expr[ContravariantK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new ContravariantK[Alg] {
        def contramapK[F[_], G[_]](af: Alg[F])(fk: G ~> F): Alg[G] =
          ${ capture('af, 'fk) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res

  @experimental def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](eaf: Expr[Alg[F]], efk: Expr[G ~> F])(using Quotes): Expr[Alg[G]] =
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
            // Cokleisli case is handled here
            // tr is Cokleisli; inner is G :: A :: B; innerTail is A :: B
            case AppliedType(tr, inner @ _ :: innerTail) if tr.baseClasses.contains(Symbol.classSymbol(classNameCokleisli)) =>
              // Build a typeRepr for
              // ContravariantK[[W[_]] =>> Cokleisli[W, A, B]]
              // A and B are in the innerTail
              val contravariantKTypeRepr =
                TypeRepr
                  .of[ContravariantK]
                  .appliedTo(
                    TypeLambda(
                      List("W"),
                      (tl: TypeLambda) =>
                        List(
                          TypeBounds(
                            TypeRepr.of[Nothing],
                            TypeLambda(
                              List("_"),
                              _ =>
                                List(
                                  TypeBounds(
                                    TypeRepr.of[Nothing],
                                    TypeRepr.of[Any]
                                  )
                                ),
                              _ => TypeRepr.of[Any]
                            )
                          )
                        ),
                      (tl: TypeLambda) => AppliedType(tr, tl.param(0) :: innerTail)
                    )
                  )

              val instanceK = summon(contravariantKTypeRepr)

              val methodArgsApply =
                argss.flatten
                  .collect { case t: Term => t }
                  .foldLeft(List.empty[Term]) {
                    // the first argument
                    case (Nil, term) => List(Apply(Select(eaf.asTerm, method), List(term)))
                    // all the rest
                    case (list, term) => List(Apply(list.head, List(term)))
                  }

              Some(
                Apply(
                  Select.overloaded(
                    instanceK,
                    "contramapK",
                    List(TypeRepr.of[F], TypeRepr.of[G]),
                    methodArgsApply
                  ),
                  List(efk.asTerm)
                )
              )

            case at: AppliedType => report.errorAndAbort("Derive works with simple algebras only.")

            case inner =>
              val instanceK = summon(typeReprFor[ApplyK, IdK](inner :: Nil))

              Some(
                Apply(
                  Select(eaf.asTerm, method),
                  argss.flatMap {
                    _.collect { case term: Term =>
                      Apply(
                        Select.overloaded(
                          instanceK,
                          "mapK",
                          List(TypeRepr.of[G], TypeRepr.of[F]),
                          List(term)
                        ),
                        List(efk.asTerm)
                      )
                    }
                  }
                )
              )
      )
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val expr   = Block(List(clsDef), newCls).asExpr

    // println("============")
    // println(expr.show)
    // println("============")
    expr.asExprOf[Alg[G]]
