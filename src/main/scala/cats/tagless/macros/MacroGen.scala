package cats.tagless.macros

import quoted.*

import scala.annotation.experimental
import compiletime.asMatchable

object MacroGen:
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
          val tpeRepr       = TypeRepr.of(using typedTree.tpe.asType)
          val namesWithTpes = clauses.map(_.params.collect { case v: ValDef => (v.name, v.tpt.tpe) }.unzip)

          val methodType =
            // nullary methods
            if clauses.isEmpty || namesWithTpes.isEmpty then ByNameType(tpeRepr)
            else
              // In case of more than a single nested list it is a curried method.
              // The idea is to foldRight it recursively nesting the result types.
              // The most right should point to the actual method result type, and every other points to the previous,
              // for more details, see: https://github.com/lampepfl/dotty/blob/3ad97df9b5e7ff6adf9952f0efc7f2df813ba395/compiler/src/dotty/tools/dotc/semanticdb/TypeOps.scala#L90-L94
              namesWithTpes
                .foldRight(Option.empty[MethodType]) { case ((nms, tps), acc) =>
                  acc match
                    case None          => Some(MethodType(nms)(_ => tps, _ => tpeRepr))
                    case Some(resType) => Some(MethodType(nms)(_ => tps, _ => resType))
                }
                .get

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

  
