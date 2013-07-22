package org.ucombinator.experimental

// TODO consider making this a case class
class AbstractState(prog: AbstractProgram)(s: List[AbstractStatement], rho: Map[AbstractVariable, AbstractValue], t: Map[AbstractVariable, Boolean], ct: Set[List[AbstractStatement]]) {
  val statements = s
  val env = rho
  val taintedVars = t
  val contextTaint = ct
  val program = prog

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: Set[AbstractState] = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = ct.filter((source) => program.influence(source).contains(s))
      s.head match {
        case AbstractLabelStatement(id, l) => Set(new AbstractState(program)(s.tail, env, t, ctPrime))
        case AbstractGotoStatement(id, l) => Set(new AbstractState(program)(program.lookup(l), env, t, ctPrime))

        case AbstractAssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = t + Pair(v, program.tainted(e, t) || !(ct.isEmpty))
          Set(new AbstractState(program)(s.tail, envPrime, tPrime, ctPrime))
        }

        case AbstractIfStatement(id, e, l) => {
          val condResult = program.eval(e, env)
          val statementListSet = Set(s)
          val fallThrough = if (AbstractValues.zero.contains(condResult)) Set(s.tail) else statementListSet.empty
          val jump = if (AbstractValues.positive.contains(condResult)) Set(program.lookup(l)) else statementListSet.empty
          val sPrimes = fallThrough | jump
          val ctPrimePrime = if (program.tainted(e, t)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          val stateSet = Set(this)
          sPrimes.foldLeft(stateSet.empty)((states, sPrime) => states + new AbstractState(program)(sPrime, env, t, ctPrimePrime))
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + s.head)
      }
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case as: AbstractState => (program equals as.program) && (statements equals as.statements) && (env equals as.env) && (taintedVars equals as.taintedVars) && (contextTaint equals as.contextTaint)
      case _ => false
    }
  }

  def isEnd: Boolean = s.isEmpty

}

object AbstractStateFactory {
  def empty: AbstractState = AbstractProgramFactory.empty.firstState
}