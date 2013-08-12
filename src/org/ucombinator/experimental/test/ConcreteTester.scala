package org.ucombinator.experimental.test

import org.ucombinator.experimental.ConcreteState
import org.ucombinator.experimental.ConcreteProgram
import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.Value
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.ConcreteAnalyzer

object ConcreteTester extends Tester {
  override def tests: Unit = {
    simpleTaint
    arithmetic
    implicitFlow
    mustReach
    loop
  }

  private def definedAndEqual[A, B](key: A, map: Map[A, B], value: B): Boolean = {
    if (map isDefinedAt key) {
      map(key) == value
    } else {
      false
    }
  }

  private def simpleTaint: Unit = {
    val code = "(:= y x)(:= z 3)"
    val result = ConcreteAnalyzer.analyze(code)
    val finalState = result.finalState
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "simpleTaint: no context taint")
    test(taintedVars contains Variable("x"), "simpleTaint: x is tainted")
    test(taintedVars contains Variable("y"), "simpleTaint: y is tainted")
    test(!(taintedVars contains Variable("z")), "simpleTaint: non-tainted variable is not tainted")
    test(!(taintedVars contains Variable("a")), "simpleTaint: non-existent variable is not tainted")
    test(finalState.env(Variable("x")) == finalState.env(Variable("y")), "simpleTaint: x == y")
    test(finalState.program.lastLineNumber == 2, "simpleTaint: line count is correct")
  }

  private def arithmetic: Unit = {
    val code = "(:= add (+ 1 2))(:= mult (* 4 6))(:= compeq (= 5 5))(:= compneq (= 8 3))"
    val result = ConcreteAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    val finalState = result.finalState
    val env = finalState.env

    test(definedAndEqual(Variable("add"), env, Value(3)), "arithmetic: addition")
    test(definedAndEqual(Variable("mult"), env, Value(24)), "arithmetic: multiplication")
    test(definedAndEqual(Variable("compeq"), env, Value(1)), "arithmetic: comparison (equal)")
    test(definedAndEqual(Variable("compneq"), env, Value(0)), "arithmetic: comparison (not equal)")
  }
  
  private def mustReach: Unit = {
    val code = "(:= y x)(:= z 1)(label _begin)(if (= x 1) _f)(:= z (+ z 1))(goto _end)(label _f)(:= z 0)(label _end)(if (= z 1) _begin)(:= y 2)"
    val program = new ConcreteProgram(ToyParser.applyStmts(code))
    
    test(program.mustReach(0) equals Set(1,2,3,8,9), "mustReach(0)")
    test(program.mustReach(1) equals Set(2,3,8,9), "mustReach(1)")
    test(program.mustReach(2) equals Set(3,8,9), "mustReach(2)")
    test(program.mustReach(3) equals Set(8,9), "mustReach(3)")
    test(program.mustReach(4) equals Set(5,8,9), "mustReach(4)")
    test(program.mustReach(5) equals Set(8,9), "mustReach(5)")
    test(program.mustReach(6) equals Set(7,8,9), "mustReach(6)")
    test(program.mustReach(7) equals Set(8,9), "mustReach(7)")
    test(program.mustReach(8) equals Set(9), "mustReach(8)")
    test(program.mustReach(9) equals Set.empty, "mustReach(9)")
    test(program.mustReach(10) equals Set(11), "mustReach(10)")
    test(program.mustReach(11) equals Set.empty, "mustReach(11)")
  }

  private def implicitFlow: Unit = {
    val code = "(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)"
    val result = ConcreteAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    val finalState = result.finalState
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars contains Variable("x"), "implicitFlow: x is tainted")
    test(!(taintedVars contains Variable("y")), "implicitFlow: y is not tainted (strong update)")
    test(taintedVars contains Variable("z"), "implicitFlow: z is tainted (implicit flow)")
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val result = ConcreteAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    val finalState = result.finalState
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars contains Variable("x"), "simpleTaint: x is tainted")
    test(!(taintedVars contains Variable("y")), "simpleTaint: y is not tainted")
  }
}