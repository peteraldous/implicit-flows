package org.ucombinator.experimental.test

import scala.util.Try

import org.ucombinator.experimental.AbstractAnalyzer
import org.ucombinator.experimental.AbstractProgram
import org.ucombinator.experimental.AbstractState
import org.ucombinator.experimental.AbstractValue
import org.ucombinator.experimental.AbstractValues
import org.ucombinator.experimental.AbstractVariable
import org.ucombinator.experimental.p
import org.ucombinator.experimental.z
import org.ucombinator.experimental.zp

object AbstractTester extends Tester {
  override def tests: Unit = {
    simpleTaint
    arithmetic
    implicitFlow
    eval
    fixedPoint
    loop
  }

  private def undefOrFalse[A](key: A, map: Map[A, Boolean]): Boolean = {
    if (map isDefinedAt key) {
      !map(key)
    } else {
      true
    }
  }

  private def testDefined(fun: (AbstractValue, AbstractValue) => AbstractValue)(lhs: AbstractValue, rhs: AbstractValue): Boolean = {
    Try(fun(lhs, rhs)).isSuccess
  }

  private def eval: Unit = {
    val program = new AbstractProgram(List.empty)
    val testAdditionDefined = testDefined(program.abstractAdd)_
    val testMultiplicationDefined = testDefined(program.abstractMultiply)_
    val testComparisonDefined = testDefined(program.abstractCompare)_
    for (lhs <- AbstractValues.all) {
      for (rhs <- AbstractValues.all) {
        test(testAdditionDefined(lhs, rhs), "eval coverage: (+ " + lhs + " " + rhs + ")")
        test(testMultiplicationDefined(lhs, rhs), "eval coverage: (* " + lhs + " " + rhs + ")")
        test(testComparisonDefined(lhs, rhs), "eval coverage: (= " + lhs + " " + rhs + ")")
      }
    }
  }

  private def fixedPoint: Unit = {
    val program = new AbstractProgram(List.empty)
    val copy = AbstractAnalyzer.setup("(:= y x)(:= z 3)")
    val paste = AbstractAnalyzer.setup("(:= y x)(:= z 3)")
    val other = copy.next.head
    val value = 2
    val graph = Map(Pair(copy, value))

    test(graph.isDefinedAt(paste) && graph(paste) == value,
      "fixedPoint: separately instantiated key exists and has the expected value")

    test(!graph.isDefinedAt(other), "fixedPoint: different state does not match")
  }

  private def simpleTaint: Unit = {
    val code = "(:= y x)(:= z 3)"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
    val finalState = linearFinal(firstState, stateGraph)
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "simpleTaint: no context taint")
    test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
    test(taintedVars(AbstractVariable("y")), "simpleTaint: y is tainted")
    test(undefOrFalse(AbstractVariable("z"), taintedVars), "simpleTaint: non-tainted variable is not tainted")
    test(undefOrFalse(AbstractVariable("a"), taintedVars), "simpleTaint: non-existent variable is not tainted")
    test(finalState.env(AbstractVariable("x")) == finalState.env(AbstractVariable("y")), "simpleTaint: x == y")
  }

  private def linearFinal(state: AbstractState, graph: Map[AbstractState, Set[AbstractState]]): AbstractState = {
    if (graph isDefinedAt state) {
      linearFinal(graph(state).head, graph)
    } else {
      state
    }
  }

  private def arithmetic: Unit = {
    val code = "(:= add (+ 1 2))(:= mult (* 4 6))(:= compeq (= 5 5))(:= compneq (= 8 3))(:= compz (= 8 0))"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
    val finalState = linearFinal(firstState, stateGraph)
    val env = finalState.env

    test(env.isDefinedAt(AbstractVariable("add")) && env(AbstractVariable("add")) == p, "arithmetic: addition")
    test(env.isDefinedAt(AbstractVariable("mult")) && env(AbstractVariable("mult")) == p, "arithmetic: multiplication")
    test(env.isDefinedAt(AbstractVariable("compeq")) && env(AbstractVariable("compeq")) == zp, "arithmetic: comparison (equal)")
    test(env.isDefinedAt(AbstractVariable("compneq")) && env(AbstractVariable("compneq")) == zp, "arithmetic: comparison (not equal but can't determine)")
    test(env.isDefinedAt(AbstractVariable("compz")) && env(AbstractVariable("compz")) == z, "arithmetic: comparison (not equal)")
  }

  private def implicitFlow: Unit = {
    val code = "(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
    val finalState = linearFinal(firstState, stateGraph)
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
    test(!taintedVars(AbstractVariable("y")), "simpleTaint: y is not tainted (strong update)")
    test(taintedVars(AbstractVariable("z")), "simpleTaint: z is tainted (implicit flow)")
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
//    val finalState = linearFinal(firstState, stateGraph)
//    val taintedVars = finalState.taintedVars

//    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
//    test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
//    test(!taintedVars(AbstractVariable("y")), "simpleTaint: y is not tainted")
  }
}