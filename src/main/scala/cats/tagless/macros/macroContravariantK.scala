package cats.tagless.macros

import cats.tagless.*
import cats.~>
import cats.data.{Tuple2K, Cokleisli}

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

    println("-----------")
    println(res.show)
    println("-----------")
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
            // head of inner is F :: G :: rest
            case AppliedType(tr, inner @ _ :: innerTail) if tr.baseClasses.contains(Symbol.classSymbol("cats.data.Cokleisli")) =>
              val mttree = innerTail.map(tr => TypeTree.of(using tr.asType))

              val methodArgsApply =
                argss.flatten
                  .collect { case t: Term => t }
                  .foldLeft(List.empty[Term]) {
                    // the first argument
                    case (Nil, term) => List(Apply(Select(eaf.asTerm, method), List(term)))
                    // all the rest
                    case (list, term) => List(Apply(list.head, List(term)))
                  }

               // TypeRepr.typeConstructorOf()
               // val showCtor = TypeRepr.typeConstructorOf(classOf[[W[_]] =>> Cokleisli[W, String, Int]])   

              println("*********")
              println(TypeRepr.of[([W[_]] =>> Cokleisli[W, String, Int])])

              println("----")
              // HKTypeLambda(
              // List(W), 
              // List(
              //   TypeBounds(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Nothing),
              //   HKTypeLambda(List(_$12), 
              //     List(TypeBounds(
              //       TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Nothing),
              //       TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Any)
              //   )), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Any), List()))), 
              //   AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class cats)),object data),Cokleisli),List(TypeParamRef(W), TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Predef),String), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Int))))

              // AppliedType(TypeRef)
              // println(TypeRepr.typeConstructorOf(classOf[Cokleisli[?, ?, ?]]))
              val cokleisliCtor = TypeRepr.typeConstructorOf(classOf[Cokleisli[?, ?, ?]])
              
              TypeRepr.of[[W[_]] =>> W]
              println(cokleisliCtor)
              println("~~~~~~~~~~~")
              val xt: TypeRepr = TypeLambda(
                List("G[_]"),
                _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any]), TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
                (tl : TypeLambda) => cokleisliCtor.appliedTo(inner))
              // val ctorFilled = cokleisliCtor.appliedTo(TypeRepr.of[[W[_]] =>> W] :: innerTail)
              val ctorFilled = cokleisliCtor.appliedTo(inner)
              println("----")
              println(TypeRepr.of[([W[_]] =>> Cokleisli[W, String, Int])])
              println("----")
              println(ctorFilled)
              println("----")
              println(xt)
              println("----")
              println(TypeRepr.of[([W[_]] =>> Cokleisli[W, String, Int])].show)
              println("----")
              println(ctorFilled.show)
              println("----")
              println(xt.show)
              println("~~~~~~~~~~~")
              Implicits.search(xt) match
                case si: ImplicitSearchSuccess =>
                  println(s"si: $si")
                case fi => println("fi!")
              
              println("----")

              println("*********")
              val x4T =
                TypeLambda(
                  List("A","B"),
                  _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any]), TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
                  (tl : TypeLambda) => tl.param(1))
              println(x4T)
              println("*********")

              Some(
                Apply(
                  Select.overloaded(
                    TypeApply(Ref(Symbol.requiredMethod("cats.tagless.ContravariantK.catsTaglessContravariantKForCokleisli")), mttree),
                    "contramapK",
                    List(TypeRepr.of[F], TypeRepr.of[G]),
                    methodArgsApply
                  ),
                  List(efk.asTerm)
                )
              )

            case at: AppliedType =>
              report.errorAndAbort("Derive works with simple algebras only.")

            case inner =>
              val mttree = List(TypeTree.of(using inner.asType))
              Some(
                Apply(
                  Select(eaf.asTerm, method),
                  argss.flatMap {
                    _.collect { case term: Term =>
                      Apply(
                        Select.overloaded(
                          TypeApply(Ref(Symbol.requiredMethod("cats.tagless.ApplyK.catsTaglessApplyKForIdK")), mttree),
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

    println("============")
    println(expr.show)
    println("============")
    expr.asExprOf[Alg[G]]
