package org.ucombinator.experimental

class Program(s: List[Statement]) {

  def tainted(e: Expression, t: Map[Variable, Boolean]): Boolean = e match {
    case Addition(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case Multiplication(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case Comparison(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case v: Value => false
    case v: Variable => t(v)
    case _ => throw new IllegalStateException("tainted: unknown expression: " + e)
  }

  def successors(statements: List[Statement]): Set[List[Statement]] = statements match {
    case GotoStatement(tl, l) :: rest => Set(statementTable(lookup(l)))
    case s :: Nil => Set.empty
    case (s: LabelStatement) :: rest => Set(rest)
    case (s: AssignmentStatement) :: rest => Set(rest)
    case IfStatement(id, c, l) :: rest => Set(rest, statementTable(lookup(l)))
    case _ => throw new IllegalStateException("successors: Could not match statement list: " + statements)
  }

  def successors(ln: Int): Set[Int] = {
    if (ln == lastLineNumber) Set.empty else statementTable(ln).head match {
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

  def firstCond(start: Int): Int = {
    if (start == lastLineNumber) lastLineNumber else {
      statementTable(start).head match {
        case (_: LabelStatement) => firstCond(start + 1)
        case (_: AssignmentStatement) => firstCond(start + 1)
        case GotoStatement(tl, l) => firstCond(lookup(l))
        case (s: IfStatement) => start
        case _ => throw new IllegalStateException("firstCond: Could not match statements list: " + statements)
      }
    }
  }

  def firstState: ConcreteState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    ConcreteState(this, 0, Map(Pair(Variable("x"), Value(2))), Map(Pair(Variable("x"), true)), Set.empty)
  }

  case class Statics(labelTable: Map[Label, Int], statementTable: Map[Int, List[Statement]], lastLineNumber: Int)

  val statements = s
  val statics = generateTables(statements)
  val lookup = statics.labelTable
  val statementTable = statics.statementTable
  val lastLineNumber = statics.lastLineNumber
  val allStatementLists = statementTable.values.toSet

  private def generateTables(statements: List[Statement]): Statics = {
    def innerGenerateTables(statements: List[Statement], labelTable: Map[Label, Int], statementTable: Map[Int, List[Statement]], ln: Int): Statics = {
      if (statements.isEmpty)
        Statics(labelTable, statementTable, ln)
      else {
        val statement = statements.head
        val next = statements.tail
        statement match {
          case LabelStatement(ln, l) => innerGenerateTables(next, labelTable + Pair(l, ln), statementTable + Pair(ln, statements), ln + 1)
          case IfStatement(ln, cond, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case GotoStatement(ln, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case AssignmentStatement(ln, v, e) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case _ => scala.sys.error("generateTables: unknown Statement type")
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty, 0)
  }

  def descendants(s: List[Statement]): Set[List[Statement]] = {
    val succs = successors(s)
    (succs map descendants).fold(succs)((set1, set2) => set1 | set2)
  }

  def descendants(s: Int): Set[Int] = {
    def innerDescendants(s: Int, seen: Set[Int]): Set[Int] = {
      def ifNotSeen(s: Int, seen: Set[Int]): Set[Int] = {
        if (seen contains s) {
          Set.empty
        } else {
          innerDescendants(s, seen) + s
        }
      }
      if (s == lastLineNumber) Set.empty else {
        val succs = successors(s)
        if (succs.size == 1) {
          ifNotSeen(succs.head, seen + s)
        } else {
          ifNotSeen(succs.head, seen + s) | ifNotSeen(succs.tail.head, seen + s)
        }
      }
    }
    innerDescendants(s, Set.empty)
  }

  /**
   * path: finds the path beginning at start and ending at the end of the program or at a conditional.
   *
   * The path is represented as a list of lists of Statement objects. Each list includes a Statement and all statements that
   * succeed it in the order given in the source code. The lists each represent a Statement (and its successors in source code
   * order) that would be executed if an interpreter began at start; that is, the result includes each Statement (bundled
   * with its successors) in program order.
   */
  def path(start: Int): Set[Int] = {
    def innerPath(start: Int, soFar: Set[Int]): Set[Int] = {
      if (start == lastLineNumber) soFar + start else {
        val statement = statementTable(start).head
        val withStart = soFar + start
        statement match {
          case (s: LabelStatement) => innerPath(start + 1, withStart)
          case GotoStatement(tl, l) => innerPath(lookup(l), withStart)
          case (s: AssignmentStatement) => innerPath(start + 1, withStart)
          case (s: IfStatement) => withStart
          case _ => throw new IllegalStateException("path: unknown statement type")
        }
      }
    }
    innerPath(start, Set.empty)
  }

  def mustReach(s: Int, seen: Set[Int] = Set.empty): Set[Int] = {
    if (seen contains s) {
      System.err.println("warning: loop. Termination leaks are possible.")
      Set.empty
    } else {
      if (s == lastLineNumber) Set(s) else {
        statementTable(s).head match {
          case as: AssignmentStatement => mustReach(s + 1) + (s + 1)
          case ls: LabelStatement => mustReach(s + 1) + (s + 1)
          case GotoStatement(ln, l) => mustReach(lookup(l)) + lookup(l)
          case IfStatement(ln, cond, l) => mustReach(s + 1) & mustReach(lookup(l))
        }
      }
    }
  }

  def influence(s: Int): Set[Int] = {
    def innerInfl(sources: Set[Int], allSourcesSeen: Set[Int], soFar: Set[Int]): Set[Int] = {
      val (clearPaths, condPaths) = sources.partition((source) => firstCond(source) == lastLineNumber)
      if (condPaths.isEmpty) {
        val paths = clearPaths.map(path)
        val commonStatements = paths.foldLeft((0 to lastLineNumber).toSet)((subset, path) => subset & path)
        paths.foldLeft(soFar)((set, path) => set | path) -- commonStatements
      } else {
        val unexploredCondPaths = condPaths -- soFar
        /*
        if (unexploredCondPaths != condPaths) {
          println("Warning: found a loop. Termination leaks are possible.")
        }
        */
        val nextSources = unexploredCondPaths.foldLeft(clearPaths)((accumulated, condPath) => accumulated | successors(firstCond(condPath)))
        val statementsSeen = condPaths.map(path).foldLeft(soFar)((statements, thisPath) => statements | thisPath)
        innerInfl(nextSources, statementsSeen)
      }
    }
    statementTable(s).head match {
      case i: IfStatement => innerInfl(successors(s), Set.empty)
      case _ => Set.empty
    }
  }
}