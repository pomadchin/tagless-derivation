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
    println("---------")
    println(methods)
    // println(TypeRepr.of[Alg].typeSymbol.fullName)
    // println(TypeRepr.of[Alg].typeSymbol.declarations)
    // println(TypeRepr.of[Alg].typeSymbol.methodMembers)
    println("---------")

    val className = "_Anon"
    val parents   = List(TypeTree.of[Object], TypeTree.of[Alg[G]])

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree.changeOwner(cls) match {
        case DefDef(name, clauses, typedTree, _) =>
          val tpeRepr = TypeRepr.of(using typedTree.tpe.asType)

          val names   = clauses.flatMap(_.params.collect { case v: ValDef => v.name })
          val tpes    = clauses.flatMap(_.params.collect { case v: ValDef => v.tpt.tpe })

          println("decls:")
          
          println(s"names: $names")
          println(s"tpes: $tpes")
          println("decls//")

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
          // lazy val apply = Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

          println(s"method.nn: ${method.tree.show(using Printer.TreeAnsiCode)}")


          val xx = method.tree match 
            case dd @ DefDef(name, clauses, typedTree, _) =>
              val tpes    = clauses.flatMap(_.params.collect { case v: ValDef => v.tpt.tpe })
              val tpeRepr = TypeRepr.of(using typedTree.tpe.asType)


              println(s"typedTree.tpe: ${typedTree.tpe}")
              println(s"dd: ${dd}")
              println(s"name: ${name}")
              println(s"tpeRepr.asType: ${tpeRepr.asType}")

              //vtypedTree.tpe
              typedTree.tpe.simplified match
                case at @ AppliedType(_, inner :: _) =>

                  println("------")
                  println(s"inner: $inner")
                  println(s"inner.typeSymbol: ${inner.typeSymbol.typeRef}")
                  println("------")

                

                  lazy val apply = Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

                
                  val x = Some(Select.overloaded(e2.asTerm, "apply", List(inner), List(apply)))// .appliedTo(apply))
                  println("!!!!!!!!!")
                  println(x)
                  println("!!!!!!!!!")

                  x
                case _ => ???

            case _ =>
              report.errorAndAbort(s"Cannot detect type of method: ${method.name}")

          
          println(s"method.signature: ${method.signature}")
          println(s"method.toString: ${method.name}")
          println(s"method.termRef.typeSymbol: ${method.termRef.typeSymbol}")
          println(s"argss.headOption.getOrElse(Nil).collect { case t: Term => t }: ${argss.headOption.getOrElse(Nil).collect { case t: Term => t }}")
          lazy val apply = Apply(Select(e1.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })
          // val applye2 = Apply(Select(e2.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })


          // val e2appl = Some(e2.asTerm.appliedTo(apply))

          // println("&&&&&&")
          // println(e2appl)
          // println("&&&&&&")

          // // val x = Some(
          // //   Apply(e2.asTerm, List(apply)) // <- тут поменять над - на вызов `e2.apply()` видимо
          // // )

          // argss

          // val x = Some(Select.overloaded(e2.asTerm, "apply", List(TypeRepr.of[Int]), List(apply)))// .appliedTo(apply))
          val x = Some(Select.overloaded(e2.asTerm, "apply", List(TypeRepr.of[Int]), List(apply)))// .appliedTo(apply))
          println(x)
          // x
          // println(apply)
          // None
          // x
          xx
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
