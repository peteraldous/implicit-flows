package org.ucombinator.experimental

case class ConcreteState(program: Program, statements: List[Statement], env: Map[Variable, Value], taintedVars: Map[Variable, Boolean], contextTaint: Set[List[Statement]]) {

  override def toString = statements.head.toString

  def next: ConcreteState = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => program.influence(source).contains(statements))
      statements.head match {
        case LabelStatement(id, l) => ConcreteState(program, statements.tail, env, taintedVars, ctPrime)
        case GotoStatement(id, l) => ConcreteState(program, program.lookup(l), env, taintedVars, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = taintedVars + Pair(v, program.tainted(e, taintedVars) || !(contextTaint.isEmpty))
          ConcreteState(program, statements.tail, envPrime, tPrime, ctPrime)
        }

        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, env) == Value(0)) {
            statements.tail
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          ConcreteState(program, sPrime, env, taintedVars, ctPrimePrime)
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + statements.head)
      }
    }
  }

  def isEnd: Boolean = statements.isEmpty
}