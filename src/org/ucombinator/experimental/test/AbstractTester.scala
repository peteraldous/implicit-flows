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
import org.ucombinator.experimental.ToyParser
import scala.util.Failure
import scala.util.Success

object AbstractTester extends Tester {
  override def tests: Unit = {
    simpleTaint
    arithmetic
    implicitFlow
    eval
    fixedPoint
    loop
    infiniteLoop
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
    val code = "(:= y x)(:= z 3)"
    val stmts = ToyParser.applyStmts(code, 0).map((stmt) => stmt.abstractMe)
    val stmts2 = ToyParser.applyStmts(code, 0).map((stmt) => stmt.abstractMe)
    test(stmts equals stmts2, "fixedPoint: List[AbstractStatements] equals works")
    val program = new AbstractProgram(stmts)
    val program2 = new AbstractProgram(stmts2)
    test(program equals program2, "fixedPoint: AbstractProgram equals works")
    val copy = new AbstractState(program)(stmts, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)),
      Set.empty)
    val paste = new AbstractState(program2)(stmts2, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)),
      Set.empty)
    val other = copy.next.head
    val value = 2
    val graph = Map(Pair(copy, value))

    test(graph.isDefinedAt(paste) && graph(paste) == value,
      "fixedPoint: separately instantiated key exists and has the expected value")
    test(!graph.isDefinedAt(other) || graph(other) != value, "fixedPoint: different state does not match")

    val env = Map(Pair(AbstractVariable("x"), p))
    test(env equals (env + Pair(AbstractVariable("x"), p)), "fixedPoint: adding a pair to a map that already contains it doesn't change it")

    val set = Set(copy)
    test(set contains paste, "fixedPoint: set contains the expected value")
    test(!(set contains other), "fixedPoint: set does not contain a different state")
    test(set equals (set + paste), "fixedPoint: adding a value to a set that already contains it doesn't change it")
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

  /*
   * WARNING: finds _some_ terminal state, not the only one.
   * 
   * If no state exists without a successor, the result will be an exception.
   */
  private def nonlinearFinal(state: AbstractState, graph: Map[AbstractState, Set[AbstractState]], visited: Set[AbstractState] = Set.empty): AbstractState = {
    if (visited contains state) {
      scala.sys.error("infinite loop - no final state")
    } else {
      if (graph isDefinedAt state) {
        val successors = graph(state)
        val newVisited = visited + state
        if (successors.size > 1) {
          Try(nonlinearFinal(successors.head, graph, newVisited)) match {
            case Success(fin) => fin
            case Failure(_) => nonlinearFinal(successors.tail.head, graph, newVisited)
          }
        } else {
          nonlinearFinal(successors.head, graph, newVisited)
        }
      } else {
        state
      }
    }
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
    val finalState = nonlinearFinal(firstState, stateGraph)
    val taintedVars = finalState.taintedVars

    test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
    test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
    test(!taintedVars(AbstractVariable("y")), "simpleTaint: y is not tainted")
  }
  
  private def infiniteLoop: Unit = {
    val code = "(label _forever)(goto _forever)"
    val firstState = AbstractAnalyzer.setup(code)
    val stateGraph = AbstractAnalyzer.explore(List(firstState), Map.empty)
    
    test(true, "infiniteLoop: analyzer didn't loop indefinitely")
  }
}