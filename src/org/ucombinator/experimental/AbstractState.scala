package org.ucombinator.experimental

case class AbstractState(program: AbstractProgram, statements: List[AbstractStatement], env: Map[AbstractVariable, AbstractValue], taintedVars: Map[AbstractVariable, Boolean], contextTaint: Set[List[AbstractStatement]]) {

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: Set[AbstractState] = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => program.influence(source).contains(statements))
      statements.head match {
        case AbstractLabelStatement(id, l) => Set(AbstractState(program, statements.tail, env, taintedVars, ctPrime))
        case AbstractGotoStatement(id, l) => Set(AbstractState(program, program.lookup(l), env, taintedVars, ctPrime))

        case AbstractAssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = taintedVars + Pair(v, program.tainted(e, taintedVars) || !(contextTaint.isEmpty))
          Set(AbstractState(program, statements.tail, envPrime, tPrime, ctPrime))
        }

        case AbstractIfStatement(id, e, l) => {
          val condResult = program.eval(e, env)
          val statementListSet = Set(statements)
          val fallThrough = if (AbstractValues.zero.contains(condResult)) Set(statements.tail) else statementListSet.empty
          val jump = if (AbstractValues.positive.contains(condResult)) Set(program.lookup(l)) else statementListSet.empty
          val sPrimes = fallThrough | jump
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          val stateSet = Set(this)
          sPrimes.foldLeft(stateSet.empty)((states, sPrime) => states + AbstractState(program, sPrime, env, taintedVars, ctPrimePrime))
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + statements.head)
      }
    }
  }

  def isEnd: Boolean = statements.isEmpty

}

object AbstractStateFactory {
  def empty: AbstractState = AbstractProgramFactory.empty.firstState
}