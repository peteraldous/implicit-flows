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
    descendants
    implicitFlow
    influence
    loop
  }

  private def undefOrEqual[A, B](key: A, map: Map[A, B], value: B): Boolean = {
    if (map isDefinedAt key) {
      map(key) == value
    } else {
      true
    }
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
    test(definedAndEqual(Variable("x"), taintedVars, true), "simpleTaint: x is tainted")
    test(definedAndEqual(Variable("y"), taintedVars, true), "simpleTaint: y is tainted")
    test(undefOrEqual(Variable("z"), taintedVars, false), "simpleTaint: non-tainted variable is not tainted")
    test(undefOrEqual(Variable("a"), taintedVars, false), "simpleTaint: non-existent variable is not tainted")
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

  private def descendants: Unit = {
    val code = "(:= y x)(:= z 1)(label _begin)(if (= x 1) _f)(:= z (+ z 1))(goto _end)(label _f)(:= z 0)(label _end)(if (= z 1) _begin)(:= y 2)"
    val program = new ConcreteProgram(ToyParser.applyStmts(code))

    test(program.descendants(0) equals (1 to 11).toSet, "internals: descendants(0)")
    test(program.descendants(1) equals (2 to 11).toSet, "internals: descendants(1)")
    test(program.descendants(2) equals (3 to 11).toSet, "internals: descendants(2)")
    test(program.descendants(3) equals (2 to 11).toSet - 3, "internals: descendants(3)")
    test(program.descendants(4) equals (2 to 11).toSet - 4, "internals: descendants(4)")
    test(program.descendants(5) equals (2 to 11).toSet - 5, "internals: descendants(5)")
    test(program.descendants(6) equals (2 to 11).toSet - 6, "internals: descendants(6)")
    test(program.descendants(7) equals (2 to 11).toSet - 7, "internals: descendants(7)")
    test(program.descendants(8) equals (2 to 11).toSet - 8, "internals: descendants(8)")
    test(program.descendants(9) equals (2 to 11).toSet - 9, "internals: descendants(9)")
    test(program.descendants(10) equals Set(11), "internals: descendants(10)")
    test(program.descendants(11) equals Set.empty, "internals: descendants(11)")
  }

  private def influence: Unit = {
    val code = "(:= y x)(:= z 1)(label _begin)(if (= x 1) _f)(:= z (+ z 1))(goto _end)(label _f)(:= z 0)(label _end)(if (= z 1) _begin)(:= y 2)"
    val program = new ConcreteProgram(ToyParser.applyStmts(code))

    test(program.influence(0) equals Set.empty, "influence of assignment is empty")
    test(program.influence(5) equals Set.empty, "influence of goto is empty")
    test(program.influence(2) equals Set.empty, "influence of label is empty")
    test(program.influence(3) equals (4 to 7).toSet, "influence of conditional")
  }

  private def implicitFlow: Unit = {
    val code = "(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)"
    val result = ConcreteAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    val finalState = result.finalState
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(definedAndEqual(Variable("x"), taintedVars, true), "implicitFlow: x is tainted")
    test(definedAndEqual(Variable("y"), taintedVars, false), "implicitFlow: y is not tainted (strong update)")
    test(definedAndEqual(Variable("z"), taintedVars, true), "implicitFlow: z is tainted (implicit flow)")
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val result = ConcreteAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    val finalState = result.finalState
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(definedAndEqual(Variable("x"), taintedVars, true), "simpleTaint: x is tainted")
    test(definedAndEqual(Variable("y"), taintedVars, false), "simpleTaint: y is not tainted")
  }
}