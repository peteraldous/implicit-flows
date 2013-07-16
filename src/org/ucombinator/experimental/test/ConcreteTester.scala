package org.ucombinator.experimental.test

import org.ucombinator.experimental.ConcreteState
import org.ucombinator.experimental.Program
import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.Value
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.ConcreteAnalyzer

object ConcreteTester extends Tester {
  override def tests: Unit = {
    simpleTaint
    arithmetic
    implicitFlow
    loop
  }
  
  private def undefOrFalse[A](key: A, map: Map[A, Boolean]): Boolean = {
    if (map isDefinedAt key) {
      !map(key)
    } else {
      true
    }
  }
  
  private def simpleTaint: Unit = {
    val firstState = ConcreteAnalyzer.setup("(:= y x)(:= z 3)")
    val stateGraph = ConcreteAnalyzer.explore(firstState, Map.empty)
    val finalState = ConcreteAnalyzer.finalState(firstState, stateGraph)
    val taintedVars = finalState.taintedVars
    
    test(finalState.contextTaint.isEmpty, "simpleTaint: no context taint")
    test(taintedVars(Variable("x")), "simpleTaint: x is tainted")
    test(taintedVars(Variable("y")), "simpleTaint: y is tainted")
    test(undefOrFalse(Variable("z"), taintedVars), "simpleTaint: non-tainted variable is not tainted")
    test(undefOrFalse(Variable("a"), taintedVars), "simpleTaint: non-existent variable is not tainted")
    test(finalState.env(Variable("x")) == finalState.env(Variable("y")), "simpleTaint: x == y")
  }
  
  private def arithmetic: Unit = {
    val firstState = ConcreteAnalyzer.setup("(:= add (+ 1 2))(:= mult (* 4 6))(:= compeq (= 5 5))(:= compneq (= 8 3))")
    val stateGraph = ConcreteAnalyzer.explore(firstState, Map.empty)
    val finalState = ConcreteAnalyzer.finalState(firstState, stateGraph)
    val env = finalState.env

    test(env.isDefinedAt(Variable("add")) && env(Variable("add")) == Value(3), "arithmetic: addition")
    test(env.isDefinedAt(Variable("mult")) && env(Variable("mult")) == Value(24), "arithmetic: multiplication")
    test(env.isDefinedAt(Variable("compeq")) && env(Variable("compeq")) == Value(1), "arithmetic: comparison (equal)")
    test(env.isDefinedAt(Variable("compneq")) && env(Variable("compneq")) == Value(0), "arithmetic: comparison (not equal)")
  }
  
  private def implicitFlow: Unit = {
    val firstState = ConcreteAnalyzer.setup("(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)")
    val stateGraph = ConcreteAnalyzer.explore(firstState, Map.empty)
    val finalState = ConcreteAnalyzer.finalState(firstState, stateGraph)
    val taintedVars = finalState.taintedVars
    
    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars(Variable("x")), "simpleTaint: x is tainted")
    test(!taintedVars(Variable("y")), "simpleTaint: y is not tainted (strong update)")
    test(taintedVars(Variable("z")), "simpleTaint: z is tainted (implicit flow)")
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val firstState = ConcreteAnalyzer.setup(code)
    val stateGraph = ConcreteAnalyzer.explore(firstState, Map.empty)
    val finalState = ConcreteAnalyzer.finalState(firstState, stateGraph)
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars(Variable("x")), "simpleTaint: x is tainted")
    test(!taintedVars(Variable("y")), "simpleTaint: y is not tainted")
  }
}