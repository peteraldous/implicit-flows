package org.ucombinator.experimental

class ConcreteState(program: Program)(s: List[Statement], rho: Map[Variable, Value], t: Map[Variable, Boolean], ct: Set[List[Statement]]) {
  val statements = s
  val env = rho
  val taintedVars = t
  val contextTaint = ct

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: ConcreteState = {
    if (s.isEmpty) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = ct.filter((source) => program.influence(source).contains(s))
      s.head match {
        case LabelStatement(id, l) => new ConcreteState(program)(s.tail, env, t, ctPrime)
        case GotoStatement(id, l) => new ConcreteState(program)(program.lookup(l), env, t, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val pPrime = env + Pair(v, program.eval(e, env))
          val tPrime = t + Pair(v, program.tainted(e, t) || !(ct.isEmpty))
          new ConcreteState(program)(s.tail, pPrime, tPrime, ctPrime)
        }

        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, env) == Value(0)) {
            s.tail
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, t)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          new ConcreteState(program)(sPrime, env, t, ctPrimePrime)
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + s.head)
      }
    }
  }

  //  def abstractMe: AbstractState = {
  // 
  //  }

  def isEnd: Boolean = s.isEmpty
}