package cats.tagless.macros

import cats.tagless.*

object Derive:
  inline def functorK[Alg[_[_]]]: FunctorK[Alg]         = macroFunctorK.derive[Alg]
  inline def semigroupalK[Alg[_[_]]]: SemigroupalK[Alg] = macroSemigroupalK.derive[Alg]
  inline def applyK[Alg[_[_]]]: ApplyK[Alg]             = macroApplyK.derive[Alg]

  // inline def contravariantK[Alg[_[_]]]: ContravariantK[Alg] = macroContravariantK[Alg]
  // inline def invariantK[Alg[_[_]]]: InvariantK[Alg] = macroInvariantK[Alg]
