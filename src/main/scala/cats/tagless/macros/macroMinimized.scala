package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.{~>, Id}
import cats.data.Tuple2K

import scala.annotation.experimental
import compiletime.asMatchable

object macroMinimized:
  import Utils.*

  inline def derive[Alg[_[_]], G[_]] = ${ instanceK[Alg, G] }

  @experimental def instanceK[Alg[_[_]]: Type, G[_]: Type](using Quotes): Expr[Alg[G]] =
    import quotes.reflect.*

    def res = capture[Alg, G]

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res

  @experimental def capture[Alg[_[_]]: Type, G[_]: Type](using Quotes): Expr[Alg[G]] =
    import quotes.reflect.*
    val className = "$anon()"

    println("*******")
    println(TypeTree.of[Alg[G]])
    println("*******")

    TypeRepr.of[G].typeSymbol
    val treef = TypeTree.of(using
      AppliedType(
        TypeTree.ref(TypeTree.of[Alg[G]].tpe.simplified.typeSymbol).tpe,
        List(TypeRepr.of[G])
      ).asType
    )

    println("|||||||||||||||")
    println(TypeRepr.of[G].dealias.show)
    println(TypeRepr.of[Id].show)
    println("|||||||||||||||")

    var trr: AppliedType | Null = null
    TypeTree
      .of[Alg[G]]
      .match
        case i: Inferred =>
          println("__________")
          println(TypeTree.of[Alg[Id]].tpe.simplified.typeSymbol)
          println()
          println(TypeTree.ref(TypeTree.of[Alg[Id]].tpe.simplified.typeSymbol))

          val rr: AppliedType = TypeTree.of[Alg[G]].tpe.simplified match
            case AppliedType(repr, list) =>
              AppliedType(TypeTree.ref(TypeTree.of[Alg[G]].tpe.simplified.typeSymbol).tpe, list)

          trr = rr

          println("*******")
          println(rr)
          println
          println(TypeTree.of[Alg[Id]].tpe)
          println("*******")
          println
          println(i.tpe)
          println("__________")

    println(":::::::::::::::")
    println(TypeTree.of[Alg[G]].tpe.=:=(TypeTree.of[Alg[Id]].tpe))
    println(":::::::::::::::")

    val parents = List(TypeTree.of[Object], TypeTree.of[Alg[G]]) // TypeTree.of(using TypeTree.of[Alg[G]].tpe.simplified.asType)
    val decls   = definedMethodsInTypeWithOwner[Alg]

    // TypeTree.of()

    println("-------------")
    // TypeTree.of[Alg[G]]

    // println("~~~~~~~~~")
    // println(TypeTree.of[Alg[G]])
    // println(TypeTree.of[Alg[Option]])
    // println(TypeTree.of[Alg[Id]])
    // println("~~~~~~~~~")

    println(TypeTree.of[Alg[G]].show)
    println(TypeTree.of[Alg[Id]].show)
    println(TypeTree.of[Alg[Option]].show)
    println("-------------")

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map(method => (method, method.tree)).collect { case (method, DefDef(_, _, typedTree, _)) =>
      DefDef(method, argss => Some('{ 42 }.asTerm))
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val expr   = Block(List(clsDef), newCls).asExpr

    println("============")
    println(expr.show)
    println("============")
    expr.asExprOf[Alg[G]]
