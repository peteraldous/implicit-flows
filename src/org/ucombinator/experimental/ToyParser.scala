package org.ucombinator.experimental

import scala.util.parsing.combinator._

object ToyParser extends RegexParsers {

  // TODO come up with a way to track line numbers
  def labelStatement: Parser[Statement] = "(label" ~> label <~ ")" ^^ { LabelStatement(1, _) }
  def gotoStatement: Parser[Statement] = "(goto" ~> label <~ ")" ^^ { GotoStatement(1, _) }
  def assignStatement: Parser[Statement] = "(:=" ~> variable ~ expr <~ ")" map {
    case variable ~ expression => AssignmentStatement(1, variable, expression)
    case _ => scala.sys.error("could not match assignment statement")
  }
  def condStatement: Parser[Statement] = "(if" ~> expr ~ label <~ ")" map {
    case expression ~ label => IfStatement(1, expression, label)
    case _ => scala.sys.error("could not match if statement")
  }

  def label: Parser[Label] = "[a-z]*".r ^^ { new Label(_) }
  def variable: Parser[Variable] = "[a-z]*".r ^^ { new Variable(_) }
  def expr: Parser[Expression] = addition | multiplication | comparison | variable | value

  def addition: Parser[Addition] = "(+" ~> expr ~ expr <~ ")" ^^ { (result) => Addition(result._1, result._2) }
  def multiplication: Parser[Multiplication] = "(*" ~> expr ~ expr <~ ")" ^^ { (result) => Multiplication(result._1, result._2) }
  def comparison: Parser[Comparison] = "(=" ~> expr ~ expr <~ ")" ^^ { (result) => Comparison(result._1, result._2) }
  def value: Parser[Value] = """\d+""".r ^^ { (integer) => Value(integer.toInt) }

  def stmt: Parser[Statement] = labelStatement | gotoStatement | assignStatement | condStatement

  def apply(input: String): Statement = parseAll(stmt, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

// TODO test, test, test