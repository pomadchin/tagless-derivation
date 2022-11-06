package cats.tagless.macros

import quoted.*

object Utils:
  def methodApply[Alg[_[_]]: Type, F[_]: Type](e: Expr[Alg[F]])(using Quotes): (quotes.reflect.Symbol, List[List[quotes.reflect.Tree]]) => quotes.reflect.Term =
    (method, argss) =>
      import quotes.reflect.*
      // method with no parentheses
      if argss.isEmpty then Select(e.asTerm, method)
      else Apply(Select(e.asTerm, method), argss.headOption.getOrElse(Nil).collect { case t: Term => t })

  def memberSymbolsAsSeen[Alg[_[_]]: Type, F[_]: Type](using Quotes): quotes.reflect.Symbol => List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    clz =>
      val algFApplied = TypeRepr.of[Alg[F]]
      val methods     = definedMethodsInTypeSym(using quotes)(clz)
      methods.map { method =>
        val asSeenApplied = algFApplied.memberType(method)
        Symbol.newMethod(
          clz,
          method.name,
          asSeenApplied,
          flags = Flags.Method, // method.flags is Flags.Deferred | Flags.Method, we'd like to unset the Deferred flag here // method.flags. &~ Flags.Deferred?
          privateWithin = method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol)
        )
      }

  // https://github.com/lampepfl/dotty/issues/11685
  def definedMethodsInTypeSym(using Quotes): quotes.reflect.Symbol => List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    (cls: quotes.reflect.Symbol) =>
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
