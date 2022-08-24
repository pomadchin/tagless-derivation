package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>
import cats.data.Tuple2K

import scala.annotation.experimental
import compiletime.asMatchable

object macroContravariantK:
  import Utils.*

  inline def derive[Alg[_[_]]] = ${ contravariantK[Alg] }

  @experimental def contravariantK[Alg[_[_]]: Type](using Quotes): Expr[ContravariantK[Alg]] =
    import quotes.reflect.*

    val res = '{
      new ContravariantK[Alg] {
        def contramapK[F[_], G[_]](af: Alg[F])(gk: G ~> F): Alg[G] =
          ${ capture('af, 'gk) }
      }
    }

    println("-----------")
    println(res.show)
    println("-----------")
    res

//   {
//   final class $anon extends AnyRef with cats.tagless.ContravariantK[[F[_]]com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[F]] {
//     def <init>(): <$anon: cats.tagless.ContravariantK[[F[_]]com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[F]]> = {
//       $anon.super.<init>();
//       ()
//     };
//     override def contramapK[F[_] >: [_]Nothing <: [_]Any, G[_] >: [_]Nothing <: [_]Any](af: com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[F])(fk: G ~> F): com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[G] = {
//       final class $anon extends AnyRef with com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[G] {
//         def <init>(): <$anon: com.disneystreaming.activation.iap.lambda.kinesis.package.UserSerivice[G]> = {
//           $anon.super.<init>();
//           ()
//         };
//         override def foldSpecialized(init: String)(f: (Int, String) => Int): cats.data.Cokleisli[G,String,Int] =
//           tagless.this.InvariantK.catsTaglessContravariantKForCokleisli[String, Int].contramapK[F, G](af.foldSpecialized(init)(f))(fk);
//         override def id(id: G[Int]): Int = af.id(tagless.this.`package`.catsTaglessApplyKForIdK[Int].mapK[G, F](id)(fk))
//       };
//       new $anon()
//     }
//   };
//   new $anon()
// }

  @experimental def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](eaf: Expr[Alg[F]], egk: Expr[G ~> F])(using Quotes): Expr[Alg[G]] =
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
            // Cokleisli case is handled here
            // head of inner is F :: G :: rest
            case AppliedType(tr, inner @ _ :: tail) if tr.baseClasses.contains(Symbol.classSymbol("cats.data.Cokleisli")) =>
              // report.errorAndAbort("Cokliesli derivation does not work yet.")
              println("-------")
              println(s"tr: $tr")
              println(s"inner: $inner")
              // List(
              //   TypeRef(NoPrefix,type G),
              //   TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String),
              //   TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int))
              val mttree = tail.map(tr => TypeTree.of(using tr.asType))

              println("-------")
              // tagless.this.InvariantK.catsTaglessContravariantKForCokleisli[String, Int].contramapK[F, G](af.foldSpecialized(init)(f))(fk);
              // cats.tagless.InvariantK.catsTaglessContravariantKForCokleisli[scala.Predef.String, scala.Int].contramapK[G, F](af.foldSpecialized(init)(f))
              // cats.tagless.InvariantK.catsTaglessContravariantKForCokleisli[scala.Predef.String, scala.Int].contramapK[F, G](af.foldSpecialized(init)(f))
              println("&&&&&&&&&&&&&")
              println(
                List(Select(eaf.asTerm, method))
              )
              println(argss)
              println("&&&&&&&&&&&&&")

              // todo this should work with an arbitrary curried function arguments
              val List(argfst, argsnd) = argss.flatten.collect { case t: Term => t }

              Some(
                Apply(
                  Select.overloaded(
                    TypeApply(Ref(Symbol.requiredMethod("cats.tagless.InvariantK.catsTaglessContravariantKForCokleisli")), mttree),
                    "contramapK",
                    List(TypeRepr.of[F], TypeRepr.of[G]),
                    List(
                      Apply(
                        Apply(
                          Select(eaf.asTerm, method),
                          List(argfst)
                        ),
                        List(argsnd)
                      )
                    )
                  ),
                  List(egk.asTerm)
                )
              )

            // None

            case at: AppliedType =>
              report.errorAndAbort("Derive works with simple algebras only.")

            case inner =>
              val mttree = List(TypeTree.of(using inner.asType))
              Some(
                Apply(
                  Select(eaf.asTerm, method),
                  argss.flatMap {
                    _.collect { case t: Term =>
                      Apply(
                        Select.overloaded(
                          TypeApply(Ref(Symbol.requiredMethod("cats.tagless.ApplyK.catsTaglessApplyKForIdK")), mttree),
                          "mapK",
                          List(TypeRepr.of[G], TypeRepr.of[F]),
                          List(t)
                        ),
                        List(egk.asTerm)
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
