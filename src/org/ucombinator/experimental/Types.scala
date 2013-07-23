package org.ucombinator.experimental

case class Label(l: String)

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
  override def toString: String
}
case class LabelStatement(ln: Int, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractLabelStatement = AbstractLabelStatement(ln, l)
  override def toString: String = "\"(Label " + l + ")\""
}
case class AssignmentStatement(ln: Int, v: Variable, e: Expression) extends Statement(ln: Int) {
  override def abstractMe: AbstractAssignmentStatement = AbstractAssignmentStatement(ln, v.abstractMe, e.abstractMe)
  override def toString: String = "\"(:= " + v + e + ")\""
}
case class GotoStatement(ln: Int, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractGotoStatement = AbstractGotoStatement(ln, l)
  override def toString: String = "\"(Goto " + l + ")\""
}
case class IfStatement(ln: Int, condition: Expression, l: Label) extends Statement(ln: Int) {
  override def abstractMe: AbstractIfStatement = AbstractIfStatement(ln, condition.abstractMe, l)
  override def toString: String = "\"(If " + condition + l + ")\""
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
