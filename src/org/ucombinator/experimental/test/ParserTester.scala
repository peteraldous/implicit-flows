package org.ucombinator.experimental.test

import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.AssignmentStatement
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.Value
import org.ucombinator.experimental.LabelStatement
import org.ucombinator.experimental.Label
import org.ucombinator.experimental.GotoStatement
import org.ucombinator.experimental.IfStatement

class TestException(msg: String) extends Exception

object ParserTester extends App {
  var count = 0
  def test(result: Boolean, tag: String): Unit = {
    if (result) {
      count += 1
    } else {
      throw new TestException("TEST FAILED: " + tag)
    }
  }

  test(ToyParser.apply("(label l)") == LabelStatement(1, Label("l")), "basic label")
  test(ToyParser.apply("(goto l)") == GotoStatement(1, Label("l")), "basic goto")
  //  test(ToyParser.apply("(:= v 1)") == AssignmentStatement(1, Variable("v"), Value(1)), "basic assignment")
  //  test(ToyParser.apply("(if 1 l)") == IfStatement(1, Value(1), Label("l")), "basic conditional")

  println(count + " tests passed")
}