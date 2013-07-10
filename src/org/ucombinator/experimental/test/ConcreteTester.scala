package org.ucombinator.experimental.test

import org.ucombinator.experimental.State
import org.ucombinator.experimental.Program
import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.Value
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.Analyzer

object ConcreteTester extends Tester {
  override def tests: Unit = {
    simpleTaint
    arithmetic
    implicitFlow
  }
  
  private def undefOrFalse[A](key: A, map: Map[A, Boolean]): Boolean = {
    if (map isDefinedAt key) {
      !map(key)
    } else {
      true
    }
  }
  
  private def simpleTaint: Unit = {
    val firstState = Analyzer.setup("(:= y x)(:= z 3)")
    val stateGraph = Analyzer.explore(firstState, Map.empty)
    val finalState = Analyzer.finalState(firstState, stateGraph)
    val taintedVars = finalState.taintedVars
    
    test(finalState.contextTaint.isEmpty, "simpleTaint: no context taint")
    test(taintedVars(Variable("x")), "simpleTaint: x is tainted")
    test(taintedVars(Variable("y")), "simpleTaint: y is tainted")
    test(undefOrFalse(Variable("z"), taintedVars), "simpleTaint: non-tainted variable is not tainted")
    test(undefOrFalse(Variable("a"), taintedVars), "simpleTaint: non-existent variable is not tainted")
    test(finalState.env(Variable("x")) == finalState.env(Variable("y")), "simpleTaint: x == y")
  }
  
  private def arithmetic: Unit = {
    val firstState = Analyzer.setup("(:= add (+ 1 2))(:= mult (* 4 6))(:= compeq (= 5 5))(:= compneq (= 8 3))")
    val stateGraph = Analyzer.explore(firstState, Map.empty)
    val finalState = Analyzer.finalState(firstState, stateGraph)
    val env = finalState.env

    test(env.isDefinedAt(Variable("add")) && env(Variable("add")) == Value(3), "arithmetic: addition")
    test(env.isDefinedAt(Variable("mult")) && env(Variable("mult")) == Value(24), "arithmetic: multiplication")
    test(env.isDefinedAt(Variable("compeq")) && env(Variable("compeq")) == Value(1), "arithmetic: comparison (equal)")
    test(env.isDefinedAt(Variable("compneq")) && env(Variable("compneq")) == Value(0), "arithmetic: comparison (not equal)")
  }
  
  private def implicitFlow: Unit = {
    val firstState = Analyzer.setup("(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)")
    val stateGraph = Analyzer.explore(firstState, Map.empty)
    val finalState = Analyzer.finalState(firstState, stateGraph)
    val taintedVars = finalState.taintedVars
    
    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars(Variable("x")), "simpleTaint: x is tainted")
    test(!taintedVars(Variable("y")), "simpleTaint: y is not tainted (strong update)")
    test(taintedVars(Variable("z")), "simpleTaint: z is tainted (implicit flow)")
  }
}