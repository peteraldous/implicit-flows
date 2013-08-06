package org.ucombinator.experimental

class ConcreteProgram(s: List[Statement]) {

  def tainted(e: Expression, t: Map[Variable, Boolean]): Boolean = e match {
    case Addition(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case Multiplication(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case Comparison(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case v: Value => false
    case v: Variable => t(v)
    case _ => throw new IllegalStateException("tainted: unknown expression: " + e)
  }

  def successors(ln: Int): Set[Int] = {
    if (ln == lastLineNumber) Set.empty else statementTable(ln) match {
      case l: LabelStatement => Set(ln + 1)
      case a: AssignmentStatement => Set(ln + 1)
      case GotoStatement(ln, l) => Set(lookup(l))
      case IfStatement(ln, e, l) => Set(lookup(l), ln + 1)
      case _ => scala.sys.error("successors: unknown statement type")
    }
  }

  def eval(e: Expression, p: Map[Variable, Value]): Value = e match {
    case Addition(lhs, rhs) => new Value(eval(lhs, p).v + eval(rhs, p).v)
    case Multiplication(lhs, rhs) => new Value(eval(lhs, p).v * eval(rhs, p).v)
    case Comparison(lhs, rhs) => new Value(if (eval(lhs, p).v == eval(rhs, p).v) 1 else 0)
    case v: Variable => p(v)
    case v: Value => v
    case _ => throw new IllegalStateException("eval: Could not match expression: " + e)
  }

  def firstState: ConcreteState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    ConcreteState(ConcreteProgram.this, 0, Map(Pair(Variable("x"), Value(2))), Map(Pair(Variable("x"), true)), Set.empty)
  }

  case class Statics(labelTable: Map[Label, Int], statementTable: Map[Int, Statement], lastLineNumber: Int)

  val statements = s
  val statics = generateTables(statements)
  val lookup = statics.labelTable
  val statementTable = statics.statementTable
  val lastLineNumber = statics.lastLineNumber
  val allStatementLists = statementTable.values.toSet

  private def generateTables(statements: List[Statement]): Statics = {
    def innerGenerateTables(statements: List[Statement], labelTable: Map[Label, Int], statementTable: Map[Int, Statement], ln: Int): Statics = {
      if (statements.isEmpty)
        Statics(labelTable, statementTable, ln)
      else {
        val statement = statements.head
        val next = statements.tail
        statement match {
          case LabelStatement(ln, l) => innerGenerateTables(next, labelTable + Pair(l, ln), statementTable + Pair(ln, statement), ln + 1)
          case IfStatement(ln, cond, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case GotoStatement(ln, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case AssignmentStatement(ln, v, e) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case _ => scala.sys.error("generateTables: unknown Statement type")
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty, 0)
  }

  // TODO: memoize
  def mustReach(s: Int, seen: Set[Int] = Set.empty): Set[Int] = {
    if (seen contains s) {
      //      System.err.println("warning: loop. Termination leaks are possible.")
      Set.empty
    } else {
      if (s == lastLineNumber) Set.empty else {
        val nextSeen = seen + s
        statementTable(s) match {
          case as: AssignmentStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case ls: LabelStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case GotoStatement(ln, l) => mustReach(lookup(l), nextSeen) + lookup(l)
          case IfStatement(ln, cond, l) => mustReach(s + 1, nextSeen) & mustReach(lookup(l), nextSeen)
        }
      }
    }
  }
}