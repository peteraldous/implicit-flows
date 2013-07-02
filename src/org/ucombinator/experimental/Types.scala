package org.ucombinator.experimental

class Label(l: String)

abstract class Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
case class Comparison(lhs: Expression, rhs: Expression) extends Expression
case class Variable(v: String) extends Expression
case class Value(v: Int) extends Expression

abstract class Statement(ln: Int)
case class LabelStatement(ln: Int, l: Label) extends Statement(ln: Int)
case class AssignmentStatement(ln: Int, v: Variable, e: Expression) extends Statement(ln: Int)
case class GotoStatement(ln: Int, l: Label) extends Statement(ln: Int)
case class IfStatement(ln: Int, condition: Expression, l: Label) extends Statement(ln: Int)
