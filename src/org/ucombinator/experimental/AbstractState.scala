package org.ucombinator.experimental

case class AbstractState(program: AbstractProgram, statements: Int, env: Map[AbstractVariable, AbstractValue], taintedVars: Map[AbstractVariable, Boolean], contextTaint: Set[Int]) {

  override def toString = program.statementTable(statements).head.toString

  def next: Set[AbstractState] = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => program.influence(source).contains(statements))
      program.statementTable(statements).head match {
        case AbstractLabelStatement(id, l) => Set(AbstractState(program, statements+1, env, taintedVars, ctPrime))
        case AbstractGotoStatement(id, l) => Set(AbstractState(program, program.lookup(l), env, taintedVars, ctPrime))

        case AbstractAssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = taintedVars + Pair(v, program.tainted(e, taintedVars) || !(contextTaint.isEmpty))
          Set(AbstractState(program, statements+1, envPrime, tPrime, ctPrime))
        }

        case AbstractIfStatement(id, e, l) => {
          val condResult = program.eval(e, env)
          val statementListSet = Set(statements)
          val fallThrough = if (AbstractValues.zero.contains(condResult)) Set(statements+1) else statementListSet.empty
          val jump = if (AbstractValues.positive.contains(condResult)) Set(program.lookup(l)) else statementListSet.empty
          val sPrimes = fallThrough | jump
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + id
          } else {
            ctPrime
          }
          val stateSet = Set(this)
          sPrimes.foldLeft(stateSet.empty)((states, sPrime) => states + AbstractState(program, sPrime, env, taintedVars, ctPrimePrime))
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + program.statementTable(statements).head)
      }
    }
  }

  def isEnd: Boolean = statements == program.lastLineNumber

}

object AbstractStateFactory {
  def empty: AbstractState = AbstractProgramFactory.empty.firstState
}