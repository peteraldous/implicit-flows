package org.ucombinator.experimental

class AbstractState(program: AbstractProgram)(s: List[AbstractStatement], rho: Map[AbstractVariable, AbstractValue], t: Map[AbstractVariable, Boolean], ct: Set[List[AbstractStatement]]) {
  val statements = s
  val env = rho
  val taintedVars = t
  val contextTaint = ct

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: Set[AbstractState] = {
    if (s.isEmpty) {
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

  /*
   *   val statements = s
  val env = rho
  val taintedVars = t
  val contextTaint = ct
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case as: AbstractState => statements == as.statements && env == as.env && taintedVars == as.taintedVars && contextTaint == as.contextTaint
      case _ => false
    }
  }

  def isEnd: Boolean = s.isEmpty

}