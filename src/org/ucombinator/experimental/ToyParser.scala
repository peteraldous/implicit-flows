/*
    Implicit Flows: a prototype taint tracking system for implicit flows
    Copyright (C) 2013   Petey Aldous <petey.aldous@utah.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

package org.ucombinator.experimental

import scala.util.parsing.combinator._

case class ParseError(msg: String) extends RuntimeException

object ToyParser extends RegexParsers {

  def labelStatement(ln: Int): Parser[Statement] = "(label" ~> label <~ ")" ^^ { LabelStatement(ln, _) }
  def gotoStatement(ln: Int): Parser[Statement] = "(goto" ~> label <~ ")" ^^ { GotoStatement(ln, _) }
  def assignStatement(ln: Int): Parser[Statement] = "(:=" ~> variable ~ expr <~ ")" map {
    case variable ~ expression => AssignmentStatement(ln, variable, expression)
    case _ => scala.sys.error("could not match assignment statement")
  }
  def condStatement(ln: Int): Parser[Statement] = "(if" ~> expr ~ label <~ ")" map {
    case expression ~ label => IfStatement(ln, expression, label)
    case _ => scala.sys.error("could not match if statement")
  }
  def functionCall(ln: Int): Parser[Statement] = "(call" ~> variable ~ expr <~ ")" map {
    case variable ~ expression => FunctionCall(ln, variable, expression)
    case _ => throw new ParseError("Bad function call")
  }

  def label: Parser[Label] = "_[a-z]+".r ^^ { Label(_) }
  def variable: Parser[Variable] = "[a-z]+".r ^^ { Variable(_) }
  def expr: Parser[Expression] = addition | multiplication | comparison | variable | value

  def value: Parser[Value] = "[0-9]+".r ^^ { (integer) => Value(integer.toInt) }
  def addition: Parser[Addition] = "(+" ~> expr ~ expr <~ ")" ^^ { (result) => Addition(result._1, result._2) }
  def multiplication: Parser[Multiplication] = "(*" ~> expr ~ expr <~ ")" ^^ { (result) => Multiplication(result._1, result._2) }
  def comparison: Parser[Comparison] = "(=" ~> expr ~ expr <~ ")" ^^ { (result) => Comparison(result._1, result._2) }

  def stmt(ln: Int): Parser[Statement] = labelStatement(ln) | gotoStatement(ln) | assignStatement(ln) | condStatement(ln)

  def applyStmt(input: String, ln: Int): Statement = parseAll(stmt(ln), input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def applyStmts(input: String, ln: Int = 0): List[Statement] = {
    def processParseResult(result: ParseResult[Statement], ln: Int): List[Statement] = result match {
      case Success(result, remainder) => result :: innerApplyStmts(remainder, ln + 1)
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    def innerApplyStmts(input: Input, ln: Int): List[Statement] = {
      if (input.atEnd) List.empty else processParseResult(parse(stmt(ln), input), ln)
    }
    processParseResult(parse(stmt(ln), input), ln)
  }

  def applyExpr(input: String): Expression = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def applyLabel(input: String): Label = parseAll(label, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}