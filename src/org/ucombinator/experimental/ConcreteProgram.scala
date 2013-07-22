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
    case GotoStatement(tl, l) :: rest => Set(lookup(l))
    case s :: Nil => Set.empty
    case (s: LabelStatement) :: rest => Set(rest)
    case (s: AssignmentStatement) :: rest => Set(rest)
    case IfStatement(id, c, l) :: rest => Set(rest, lookup(l))
    case _ => throw new IllegalStateException("successors: Could not match statement list: " + statements)
  }

  def eval(e: Expression, p: Map[Variable, Value]): Value = e match {
    case Addition(lhs, rhs) => new Value(eval(lhs, p).v + eval(rhs, p).v)
    case Multiplication(lhs, rhs) => new Value(eval(lhs, p).v * eval(rhs, p).v)
    case Comparison(lhs, rhs) => new Value(if (eval(lhs, p).v == eval(rhs, p).v) 1 else 0)
    case v: Variable => p(v)
    case v: Value => v
    case _ => throw new IllegalStateException("eval: Could not match expression: " + e)
  }

  def firstCond(statements: List[Statement], end: List[Statement]): List[Statement] = statements match {
    case Nil => List.empty
    case `end` => List.empty
    case (_: LabelStatement) :: rest => firstCond(rest, end)
    case (_: AssignmentStatement) :: rest => firstCond(rest, end)
    case GotoStatement(tl, l) :: rest => firstCond(lookup(l), end)
    case (s: IfStatement) :: rest => s :: rest
    case _ => throw new IllegalStateException("firstCond: Could not match statements list: " + statements)
  }

  def firstState: ConcreteState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    new ConcreteState(this)(statements, Map(Pair(Variable("x"), Value(2))), Map(Pair(Variable("x"), true)), Set.empty)
  }

  val statements = s
  val tables = generateTables(statements)
  val lookup = tables._1
  val conditionals = tables._2
  val allStatementLists = {
    def allSuffixes(suffixes: Set[List[Statement]], lst: List[Statement]): Set[List[Statement]] = {
      val withThis = suffixes | Set(lst)
      if (lst.isEmpty) {
        withThis
      } else {
        allSuffixes(withThis, lst.tail)
      }
    }
    allSuffixes(Set.empty, statements)
  }

  private def generateTables(statements: List[Statement]): Pair[Map[Label, List[Statement]], Map[Int, List[Statement]]] = {
    def innerGenerateTables(statements: List[Statement], labelsSoFar: Map[Label, List[Statement]], conditionalsSoFar: Map[Int, List[Statement]]): Pair[Map[Label, List[Statement]], Map[Int, List[Statement]]] = {
      if (statements.isEmpty)
        (labelsSoFar, conditionalsSoFar)
      else {
        val statement = statements.head
        statement match {
          case LabelStatement(ln, l) => innerGenerateTables(statements.tail, labelsSoFar.+((l, statements)), conditionalsSoFar)
          case IfStatement(ln, cond, l) => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar.+((ln, statements)))
          case _ => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar)
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty)
  }

  def descendants(s: List[Statement]): Set[List[Statement]] = {
    val succs = successors(s)
    (succs map descendants).fold(succs)((set1, set2) => set1 | set2)
  }

  def highestCommonDescendant(sources: Set[List[Statement]]): List[Statement] = {
    val commonDescendants = (sources map descendants).foldLeft(allStatementLists)((set1, set2) => set1 & set2)
    def firstSuffixMatch(lst: List[Statement]): Option[List[Statement]] = {
      if (commonDescendants.contains(lst)) {
        Some(lst)
      } else {
        if (lst.isEmpty) {
          None
        } else {
          lst.head match {
            case GotoStatement(ln, l) => firstSuffixMatch(lookup(l))
            case IfStatement(ln, cond, l) => {
              val fallThrough = firstSuffixMatch(lst.tail)
              if (fallThrough.isEmpty) {
                firstSuffixMatch(lookup(l))
              } else {
                fallThrough
              }
            }
            case _ => firstSuffixMatch(lst.tail)
          }
        }
      }
    }
    // get throws an error if there's nothing
    firstSuffixMatch(sources.head).get
  }

  /**
   * path: finds the path beginning at start and ending at end.
   *
   * The path is represented as a list of lists of Statement objects. Each list includes a Statement and all statements that
   * succeed it in the order given in the source code. The lists each represent a Statement (and its successors in source code
   * order) that would be executed if an interpreter began at start; that is, the result includes each Statement (bundled
   * with its successors) in program order from start to end.
   *
   * precondition: No conditional statements may exist in program order between start and end.
   */
  def path(end: List[Statement])(start: List[Statement]): List[List[Statement]] = {
    def innerPath(start: List[Statement], soFar: List[List[Statement]]): List[List[Statement]] = {
      if (start.isEmpty)
        throw new IllegalStateException("path: No more statements and the end has not been reached")
      else {
        start match {
          case `end` => soFar
          case (s: LabelStatement) :: rest => innerPath(rest, start :: soFar)
          case GotoStatement(tl, l) :: rest => innerPath(lookup(l), start :: soFar)
          case (s: AssignmentStatement) :: rest => innerPath(rest, start :: soFar)
          case (s: IfStatement) :: rest => throw new IllegalStateException("path: Conditionals are not permitted")
          case _ => throw new IllegalStateException("path: unknown statement type")
        }
      }
    }
    innerPath(start, Nil)
  }

  def influence(s: List[Statement]): Set[List[Statement]] = {
    def innerInfl(sources: Set[List[Statement]], soFar: Set[List[Statement]]): Set[List[Statement]] = {
      val sourceHCD = highestCommonDescendant(sources)
      val (clearPaths, condPaths) = sources.partition((source) => firstCond(source, sourceHCD).isEmpty)
      if (condPaths.isEmpty)
        clearPaths.map(path(sourceHCD)).foldLeft(soFar)((set, list) => set | list.toSet)
      else
        innerInfl(
          condPaths.foldLeft(clearPaths)((accumulated, condPath) => accumulated | successors(firstCond(condPath, sourceHCD))),
          condPaths.map((condPath) => path(firstCond(condPath, sourceHCD))(condPath)).foldLeft(soFar)((statements, thisPath) => statements | thisPath.toSet))
    }
    s.head match {
      case i: IfStatement => innerInfl(successors(s), Set.empty)
      case _ => Set.empty
    }
  }
}