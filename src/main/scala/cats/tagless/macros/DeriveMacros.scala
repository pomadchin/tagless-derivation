package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>

import scala.annotation.experimental
import compiletime.asMatchable

object DeriveMacros {

  def definedMethodsInType[Alg[_[_]]: Type](using Quotes): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*

    val cls = TypeRepr.of[Alg].typeSymbol

    for {
      member <- cls.methodMembers
      // is abstract method, not implemented
      if member.flags.is(Flags.Deferred)

      // TODO: is that public?
      // TODO? if member.privateWithin
      if !member.flags.is(Flags.Private)
      if !member.flags.is(Flags.Protected)
      if !member.flags.is(Flags.PrivateLocal)

      if !member.isClassConstructor
      if !member.flags.is(Flags.Synthetic)
    } yield member
  }

  inline def functorK[Alg[_[_]]] = ${ functorKGen[Alg] }

  @experimental def functorKGen[Alg[_[_]]: Type](using Quotes): Expr[FunctorK[Alg]] = {
    import quotes.reflect.*

    val res = '{
      new FunctorK[Alg] {
        def mapK[F[_], G[_]](af: Alg[F])(fk: F ~> G): Alg[G] =
          ${ capture('af, 'fk) }
      }
    }

    // println("-----------")
    // println(res.show)
    // println("-----------")
    res
  }

  @experimental
  def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](e1: Expr[Alg[F]], e2: Expr[F ~> G])(using Quotes) =
    import quotes.reflect.*
    val methods = definedMethodsInType[Alg]
    val className = "$anon()"
    val parents   = List(TypeTree.of[Object], TypeTree.of[Alg[G]])

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree.changeOwner(cls) match {
        case DefDef(name, clauses, typedTree, _) =>
          val tpeRepr = TypeRepr.of(using typedTree.tpe.asType)
          val names   = clauses.flatMap(_.params.collect { case v: ValDef => v.name })
          val tpes    = clauses.flatMap(_.params.collect { case v: ValDef => v.tpt.tpe })

          // nullary methods
          val methodType = if (clauses.isEmpty) ByNameType(tpeRepr) else MethodType(names)(_ => tpes, _ => tpeRepr)
          Symbol.newMethod(
            cls,
            name,
            methodType,
            flags = Flags.EmptyFlags /*TODO: method.flags */,
            privateWithin = method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol)
          )
        case _ =>
          report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
      }
    }

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)

    val body = cls.declaredMethods.map(method => (method, method.tree)).collect { case (method, DefDef(_, _, typedTree, _)) =>
      DefDef(
        method,
        argss =>
          typedTree.tpe.simplified.asMatchable match
            case at @ AppliedType(_, inner :: _) =>
              val apply = 
                // method with no parentheses
                if argss.isEmpty then Select(e1.asTerm, method)
                else Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })
              Some(Select.overloaded(e2.asTerm, "apply", List(inner), List(apply)))
            case _ => None
      )
    }

    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val expr     = Block(List(clsDef), newCls).asExpr

    // println("============")
    // println(expr.show)
    // println("============")
    expr.asExprOf[Alg[G]]
}
