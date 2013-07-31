package org.ucombinator.experimental

case class ConcreteState(program: ConcreteProgram, statements: Int, env: Map[Variable, Value], taintedVars: Map[Variable, Boolean], contextTaint: Set[Int]) {

  override def toString = program.statementTable(statements).head.toString

  def next: ConcreteState = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => program.influence(source).contains(statements))
      program.statementTable(statements).head match {
        case LabelStatement(id, l) => ConcreteState(program, statements+1, env, taintedVars, ctPrime)
        case GotoStatement(id, l) => ConcreteState(program, program.lookup(l), env, taintedVars, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = taintedVars + Pair(v, program.tainted(e, taintedVars) || !(contextTaint.isEmpty))
          ConcreteState(program, statements+1, envPrime, tPrime, ctPrime)
        }

        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, env) == Value(0)) {
            statements+1
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + id
          } else {
            ctPrime
          }
          ConcreteState(program, sPrime, env, taintedVars, ctPrimePrime)
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + program.statementTable(statements).head)
      }
    }
  }

  def isEnd: Boolean = statements == program.lastLineNumber
}