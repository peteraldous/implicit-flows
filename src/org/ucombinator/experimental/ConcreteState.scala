package org.ucombinator.experimental

class ConcreteState(program: Program)(s: List[Statement], p: Map[Variable, Value], t: Map[Variable, Boolean], ct: Set[List[Statement]]) {
  val statements = s
  val env = p
  val taintedVars = t
  val contextTaint = ct

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: ConcreteState = {
    if (s.isEmpty) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = ct.filter((source) => program.influence(source).contains(s))
      s.head match {
        case LabelStatement(id, l) => new ConcreteState(program)(s.tail, p, t, ctPrime)
        case GotoStatement(id, l) => new ConcreteState(program)(program.lookup(l), p, t, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val pPrime = p + Pair(v, program.eval(e, p))
          val tPrime = t + Pair(v, program.tainted(e, t) || !(ct.isEmpty))
          new ConcreteState(program)(s.tail, pPrime, tPrime, ctPrime)
        }

        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, p) == Value(0)) {
            s.tail
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, t)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          new ConcreteState(program)(sPrime, p, t, ctPrimePrime)
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