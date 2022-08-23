package cats.tagless.macros

import quoted.*

object Utils:
  def methodApply[Alg[_[_]]: Type, F[_]: Type](e: Expr[Alg[F]])(using Quotes): (quotes.reflect.Symbol, List[List[quotes.reflect.Tree]]) => quotes.reflect.Term =
    (method, argss) =>
      import quotes.reflect.*
      // method with no parentheses
      if argss.isEmpty then Select(e.asTerm, method)
      else Apply(Select(e.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

  // https://github.com/lampepfl/dotty/issues/11685
  def definedMethodsInType[Alg[_[_]]: Type](using Quotes): List[quotes.reflect.Symbol] =
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

  // returns a function that reconstructs functions of the Algebra to belong to a specific owner
  // cls => List[Symbol] function
  def definedMethodsInTypeWithOwner[Alg[_[_]]: Type](using Quotes): quotes.reflect.Symbol => List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    val methods = definedMethodsInType[Alg]

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree.changeOwner(cls) match
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

    decls
