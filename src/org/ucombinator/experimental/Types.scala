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

// TODO: make Expressions and Values and so on implement an interface for extensibility to the value system

case class Label(l: String)
case class FunctionName(name: String)
case class Function(name: FunctionName, body: Label, parameters: Expression*)
case class StackFrame(target: Int, locals: Map[Variable, Value])
case class Address(a: Int)
// states are (ln, env, store)
// configurations are (state, stack summary)
// do i need a separate store and env?
// - yes - because shadowing is possible and i don't want to merge every variable with the same name
// TODO create a function table (FQN -> Function object)
// I may need to come up with an object system for this paper

abstract class Expression {
  def abstractMe: AbstractExpression
}
case class Addition(lhs: Expression, rhs: Expression) extends Expression {
  override def abstractMe: AbstractAddition = {
    AbstractAddition(lhs.abstractMe, rhs.abstractMe)
  }
}
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression {
  override def abstractMe: AbstractMultiplication = {
    AbstractMultiplication(lhs.abstractMe, rhs.abstractMe)
  }
}
case class Comparison(lhs: Expression, rhs: Expression) extends Expression {
  override def abstractMe: AbstractComparison = {
    AbstractComparison(lhs.abstractMe, rhs.abstractMe)
  }
}
case class Variable(v: String) extends Expression {
  override def abstractMe: AbstractVariable = AbstractVariable(v)
}
case class Value(v: Int) extends Expression {
  override def abstractMe: AbstractValue = if (v < 0) n else { if (v > 0) p else z }
}

abstract class AbstractExpression
case class AbstractAddition(lhs: AbstractExpression, rhs: AbstractExpression) extends AbstractExpression
case class AbstractMultiplication(lhs: AbstractExpression, rhs: AbstractExpression) extends AbstractExpression
case class AbstractComparison(lhs: AbstractExpression, rhs: AbstractExpression) extends AbstractExpression
case class AbstractVariable(v: String) extends AbstractExpression
abstract class AbstractValue() extends AbstractExpression

case object nzp extends AbstractValue()
case object nz extends AbstractValue()
case object np extends AbstractValue()
case object zp extends AbstractValue()
case object n extends AbstractValue()
case object z extends AbstractValue()
case object p extends AbstractValue()

object AbstractValues {
  val positive = Set[AbstractValue](nzp, np, zp, p)
  val zero = Set[AbstractValue](nzp, nz, zp, z)
  val negative = Set[AbstractValue](nzp, nz, np, n)
  val all = positive | zero | negative
}

abstract class Statement(ln: Int) {
  def abstractMe: AbstractStatement
  override def toString: String = "[" + ln + "]"
}
case class LabelStatement(ln: Int, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractLabelStatement = AbstractLabelStatement(ln, l)
  override def toString: String = super.toString + "\"(Label " + l + ")\""
}
case class AssignmentStatement(ln: Int, v: Variable, e: Expression) extends Statement(ln: Int) {
  override def abstractMe: AbstractAssignmentStatement = AbstractAssignmentStatement(ln, v.abstractMe, e.abstractMe)
  override def toString: String = super.toString + "\"(:= " + v + e + ")\""
}
case class GotoStatement(ln: Int, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractGotoStatement = AbstractGotoStatement(ln, l)
  override def toString: String = super.toString + "\"(Goto " + l + ")\""
}
case class IfStatement(ln: Int, condition: Expression, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractIfStatement = AbstractIfStatement(ln, condition.abstractMe, l)
  override def toString: String = super.toString + "\"(If " + condition + l + ")\""
}
case class FunctionCall(ln: Int, fun: Variable, exps: Expression*) extends Statement(ln: Int) {
  override def abstractMe: AbstractFunctionCall = AbstractFunctionCall(ln, fun, exps map { _.abstractMe }: _*)
  override def toString: String = super.toString + "\"(" + fun + " " + exps + ")\""
}

abstract class AbstractStatement(ln: Int) {
  override def toString: String
}
case class AbstractLabelStatement(ln: Int, l: Label) extends AbstractStatement(ln: Int) {
  override def toString: String = "\"(Label " + l + ")\""
}
case class AbstractAssignmentStatement(ln: Int, v: AbstractVariable, e: AbstractExpression) extends AbstractStatement(ln: Int) {
  override def toString: String = "\"(:= " + v + e + ")\""
}
case class AbstractGotoStatement(ln: Int, l: Label) extends AbstractStatement(ln: Int) {
  override def toString: String = "\"(Goto " + l + ")\""
}
case class AbstractIfStatement(ln: Int, condition: AbstractExpression, l: Label) extends AbstractStatement(ln: Int) {
  override def toString: String = "\"(If " + condition + l + ")\""
}
case class AbstractFunctionCall(ln: Int, fun: Variable, exps: AbstractExpression*) extends AbstractStatement(ln: Int) {
  override def toString: String = "\"(" + fun + " "+ exps + ")\""
}
