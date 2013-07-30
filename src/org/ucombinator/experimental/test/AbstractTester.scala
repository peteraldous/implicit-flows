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
    val copy = AbstractState(program, 0, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)),
      Set.empty)
    val paste = AbstractState(program2, 0, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)),
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
    val result = AbstractAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    for (finalState <- result.finalStates) {
      val taintedVars = finalState.taintedVars

      test(finalState.contextTaint.isEmpty, "simpleTaint: no context taint")
      test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
      test(taintedVars(AbstractVariable("y")), "simpleTaint: y is tainted")
      test(undefOrFalse(AbstractVariable("z"), taintedVars), "simpleTaint: non-tainted variable is not tainted")
      test(undefOrFalse(AbstractVariable("a"), taintedVars), "simpleTaint: non-existent variable is not tainted")
      test(finalState.env(AbstractVariable("x")) == finalState.env(AbstractVariable("y")), "simpleTaint: x == y")
    }
  }

  private def arithmetic: Unit = {
    val code = "(:= add (+ 1 2))(:= mult (* 4 6))(:= compeq (= 5 5))(:= compneq (= 8 3))(:= compz (= 8 0))"
    val result = AbstractAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    for (finalState <- result.finalStates) {
      val env = finalState.env

      test(env.isDefinedAt(AbstractVariable("add")) && env(AbstractVariable("add")) == p, "arithmetic: addition")
      test(env.isDefinedAt(AbstractVariable("mult")) && env(AbstractVariable("mult")) == p, "arithmetic: multiplication")
      test(env.isDefinedAt(AbstractVariable("compeq")) && env(AbstractVariable("compeq")) == zp, "arithmetic: comparison (equal)")
      test(env.isDefinedAt(AbstractVariable("compneq")) && env(AbstractVariable("compneq")) == zp, "arithmetic: comparison (not equal but can't determine)")
      test(env.isDefinedAt(AbstractVariable("compz")) && env(AbstractVariable("compz")) == z, "arithmetic: comparison (not equal)")
    }
  }

  private def implicitFlow: Unit = {
    val code = "(:= y x)(if (= x 1) _f)(:= z 1)(goto _end)(label _f)(:= z 0)(label _end)(:= y 2)"
    val result = AbstractAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    for (finalState <- result.finalStates) {
      val taintedVars = finalState.taintedVars

      test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
      test(taintedVars(AbstractVariable("x")), "simpleTaint: x is tainted")
      test(!taintedVars(AbstractVariable("y")), "simpleTaint: y is not tainted (strong update)")
      test(taintedVars(AbstractVariable("z")), "simpleTaint: z is tainted (implicit flow)")
    }
  }

  private def loop: Unit = {
    val code = "(:= y 0)(label _loop)(if (= y 10) _end)(:= y (+ y 1))(goto _loop)(label _end)"
    val result = AbstractAnalyzer.analyze(code)
    val stateGraph = result.successorGraph
    for (finalState <- result.finalStates) {
      val taintedVars = finalState.taintedVars

      test(finalState.contextTaint.isEmpty, "implicitFlow: no context taint")
      test(taintedVars.getOrElse(AbstractVariable("x"), false), "simpleTaint: x is tainted")
      test(!taintedVars.getOrElse(AbstractVariable("y"), true), "simpleTaint: y is not tainted")
    }
  }

  private def infiniteLoop: Unit = {
    val code = "(label _forever)(goto _forever)"
    val result = Try(AbstractAnalyzer.analyze(code))
    val programResult = result match {
      case Success(r) => true
      case Failure(e) => false
    }

    test(programResult, "infiniteLoop: analyzer didn't loop indefinitely")
  }
}