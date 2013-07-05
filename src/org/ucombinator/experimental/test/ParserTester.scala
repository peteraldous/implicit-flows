package org.ucombinator.experimental.test

import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.AssignmentStatement
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.Value
import org.ucombinator.experimental.LabelStatement
import org.ucombinator.experimental.Label
import org.ucombinator.experimental.GotoStatement
import org.ucombinator.experimental.IfStatement
import org.ucombinator.experimental.Addition
import org.ucombinator.experimental.Multiplication
import org.ucombinator.experimental.Comparison
import org.ucombinator.experimental.Multiplication
import org.ucombinator.experimental.AssignmentStatement

object ParserTester extends App {
  var passed = 0
  var run = 0
  def test(result: Boolean, tag: String): Unit = {
    run += 1
    if (result) {
      passed += 1
    } else {
      println("TEST FAILED: " + tag)
    }
  }

  def runTests: Unit = {
    test(ToyParser.applyExpr("2") == Value(2), "basic value")
    test(ToyParser.applyExpr("29") == Value(29), "two-digit value")
    test(ToyParser.applyLabel("_l") == Label("_l"), "basic label")
    test(ToyParser.applyExpr("v") == Variable("v"), "basic variable")
    test(ToyParser.applyExpr("(+ 1 2)") == Addition(Value(1), Value(2)), "basic addition")
    test(ToyParser.applyExpr("(* 1 2)") == Multiplication(Value(1), Value(2)), "basic multiplication")
    test(ToyParser.applyExpr("(= 1 2)") == Comparison(Value(1), Value(2)), "basic comparison")
    test(ToyParser.applyExpr("(= (+ 2 3) (* 8 7))") == Comparison(Addition(Value(2), Value(3)), Multiplication(Value(8), Value(7))), "nested arithmetic")
    test(ToyParser.applyExpr("(+ 1 v)") == Addition(Value(1), Variable("v")), "variable in addition")
    test(ToyParser.applyStmt("(label _l)", 34) == LabelStatement(34, Label("_l")), "basic label")
    test(ToyParser.applyStmt("(goto _l)", 4) == GotoStatement(4, Label("_l")), "basic goto")
    test(ToyParser.applyStmt("(:= v 1)", 2) == AssignmentStatement(2, Variable("v"), Value(1)), "basic assignment")
    test(ToyParser.applyStmt("(if 1 _l)", 169) == IfStatement(169, Value(1), Label("_l")), "basic conditional")
    test(ToyParser.applyStmt("(:= y (+ 1 2))", 3) == AssignmentStatement(3, Variable("y"), Addition(Value(1), Value(2))), "nested addition in assignment")

    test(ToyParser.applyStmts("(label _l)(goto _l)", 3) == List(LabelStatement(3, Label("_l")), GotoStatement(4, Label("_l"))), "two statements")

    println(passed + " of " + run + " tests passed")
  }

  runTests
}