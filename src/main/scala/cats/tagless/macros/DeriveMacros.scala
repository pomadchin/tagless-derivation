package cats.tagless.macros

import cats.tagless.*
import quoted.*
import cats.~>

import scala.annotation.experimental

object DeriveMacros {
  // trait Foo

  // def applyK[Alg[_[_]]](using Quotes) = '{
  //   class myClass() extends Object with Foo {
  //     def foo(): Unit = println("Calling foo")
  //   }
  //   new myClass(): Foo
  // }

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

  inline def functorInvoke[T] = ${ functor[T] }

  def functor[Alg](using t: Type[Alg])(using Quotes) = {
    import quotes.reflect.*

    val tree = TypeTree.of[Alg]

    println("~~~~~~")
    println(tree)
    println(tree.tpe)
    println("~~~~~~")
    println(tree.symbol.methodMembers)
    println(tree.symbol.methodMember("id"))
    println(tree.symbol.declarations)
    println(tree.symbol.declaredFields)
    println("~~~~~~")

    // val decls = definedMethodsInType[Alg]
    // println(decls)
    println("---------")
    println(TypeRepr.of[Alg].typeSymbol.fullName)
    println("---------")

    val s = t.toString()
    Expr(s)
  }

  inline def functorKInvoke[Alg[_[_]]] = ${ functorK[Alg] }

  @experimental def functorK[Alg[_[_]]: Type](using Quotes): Expr[FunctorK[Alg]] = {
    import quotes.reflect.*

    val res = '{
      new FunctorK[Alg] {
        def mapK[F[_], G[_]](af: Alg[F])(fk: F ~> G): Alg[G] =
          ${ capture('af, 'fk) }
      }
    }

    println("***********")
    println(res.show)
    println("***********")

    res

    // val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    // val body = cls.declaredMethods.map { method => DefDef(method, argss => Some('{${r}()}.asTerm)) }
    // val clsDef = ClassDef(cls, parents, body = body)
    // val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
    // Block(List(clsDef), newCls).asExprOf[Alg]
  }

  @experimental
  def capture[Alg[_[_]]: Type, F[_]: Type, G[_]: Type](e1: Expr[Alg[F]], e2: Expr[F ~> G])(using Quotes) =
    import quotes.reflect.*
    val methods = definedMethodsInType[Alg]
    val className = "_Anon"
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

    // val ress: Term = '{"fk"}.asTerm
    val body = cls.declaredMethods.map { method =>
      DefDef(
        method,
        argss =>
          method.tree match 
            case DefDef(name, clauses, typedTree, _) =>
              typedTree.tpe.simplified match
                case at @ AppliedType(_, inner :: _) =>
                  lazy val apply = Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })
                  Some(Select.overloaded(e2.asTerm, "apply", List(inner), List(apply)))
                case _ => ???

            case _ =>
              report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
      )
    }
    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Alg[G]])
    val ee     = Block(List(clsDef), newCls).asExpr

    println("============")
    println(ee.show)
    println("============")
    ee.asExprOf[Alg[G]]

  inline def functorK2Invoke[T]: String = ${ functorK2[T] }

  def functorK2[Alg](using t: Type[Alg])(using Quotes) = {
    import quotes.reflect.*
    println("---------")
    println(TypeRepr.of[Alg].typeSymbol.fullName)
    println("---------")
    val s = t.toString()
    Expr(s)
    // val s = t match
    //   case '[Foo[?]] => "FooSuccess" //compiler error, but other two successfully match
    //   case '[Bar[?]] => "BarSuccess"
    //   case '[Baz[?]] => "BazSuccess"
    //   case _ => "Fail"
  }
}
