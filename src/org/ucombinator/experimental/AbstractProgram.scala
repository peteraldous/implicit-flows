package org.ucombinator.experimental

class AbstractProgram(s: List[AbstractStatement]) {

  def tainted(e: AbstractExpression, t: Map[AbstractVariable, Boolean]): Boolean = e match {
    case AbstractAddition(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case AbstractMultiplication(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case AbstractComparison(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case v: AbstractValue => false
    case v: AbstractVariable => t(v)
    case _ => throw new IllegalStateException("tainted: unknown expression: " + e)
  }

  def successors(ln: Int): Set[Int] = {
    if (ln == lastLineNumber) Set.empty else statementTable(ln).head match {
      case l: AbstractLabelStatement => Set(ln + 1)
      case a: AbstractAssignmentStatement => Set(ln + 1)
      case AbstractGotoStatement(ln, l) => Set(lookup(l))
      case AbstractIfStatement(ln, e, l) => Set(lookup(l), ln + 1)
      case _ => scala.sys.error("successors: unknown statement type")
    }
  }

  def abstractAdd(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      // nzp is top
      case (`nzp`, _) => nzp // 7 cases - 7 total
      case (_, `nzp`) => nzp // 6 cases - 13 total
      // if they match (assuming no overflow), the sign remains the same
      case (copy, paste) if (copy == paste) => copy // 6 cases - 19 total
      // zero doesn't change the case
      case (`z`, s) => s // 5 cases - 24 total
      case (s, `z`) => s // 5 cases - 29 total
      // this catches all cases that have n in one and p in the other
      case (lhs, rhs) if ((AbstractValues.positive.contains(rhs) && AbstractValues.negative.contains(lhs)) ||
        (AbstractValues.negative.contains(rhs) && AbstractValues.positive.contains(lhs))) => nzp
      // 16 cases - 45 total
      case (`nz`, `n`) => n // 46 total
      case (`n`, `nz`) => n // 47 total
      case (`zp`, `p`) => p // 48 total
      case (`p`, `zp`) => p // 49 total
    }
  }

  def abstractMultiply(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      case (`z`, _) => z // 7 cases  - 7 total
      case (_, `z`) => z // 6 cases  - 13 total
      case (`p`, s) => s // 6 cases  - 19 total
      case (s, `p`) => s // 5 cases  - 24 total
      case (`nzp`, s) => nzp // 5 cases  - 29 total
      case (s, `nzp`) => nzp // 4 cases  - 33 total
      case (`n`, `n`) => p // 34 total
      case (`n`, `nz`) => zp // 35 total
      case (`nz`, `n`) => zp // 36 total
      case (`n`, `zp`) => nz // 37 total
      case (`zp`, `n`) => nz // 38 total
      case (`n`, `np`) => np // 39 total
      case (`np`, `n`) => np // 40 total
      case (`nz`, `nz`) => zp // 41 total
      case (`zp`, `zp`) => zp // 42 total
      case (`np`, `np`) => np // 43 total
      case (`zp`, `np`) => nzp // 44 total
      case (`np`, `zp`) => nzp // 45 total
      case (`zp`, `nz`) => nz // 46 total
      case (`nz`, `zp`) => nz // 47 total
      case (`nz`, `np`) => nzp // 48 total
      case (`np`, `nz`) => nzp // 49 total
    }
  }

  def abstractCompare(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      // this is the only case that must be true (positive)
      case (`z`, `z`) => p
      case (lhs, rhs) => if ((AbstractValues.positive.contains(lhs) && AbstractValues.positive.contains(rhs)) ||
        (AbstractValues.zero.contains(lhs) && AbstractValues.zero.contains(rhs)) ||
        (AbstractValues.negative.contains(lhs) && AbstractValues.negative.contains(rhs))) zp else z
    }
  }

  def eval(e: AbstractExpression, rho: Map[AbstractVariable, AbstractValue]): AbstractValue = e match {
    case AbstractAddition(lhs, rhs) => abstractAdd(eval(lhs, rho), eval(rhs, rho))
    case AbstractMultiplication(lhs, rhs) => abstractMultiply(eval(lhs, rho), eval(rhs, rho))
    case AbstractComparison(lhs, rhs) => abstractCompare(eval(lhs, rho), eval(rhs, rho))
    case v: AbstractVariable => rho(v)
    case v: AbstractValue => v
    case _ => throw new IllegalStateException("eval: Could not match expression: " + e)
  }

  def firstCond(start: Int): Int = {
    if (start == lastLineNumber) lastLineNumber else {
      statementTable(start).head match {
        case (_: AbstractLabelStatement) => firstCond(start + 1)
        case (_: AbstractAssignmentStatement) => firstCond(start + 1)
        case AbstractGotoStatement(tl, l) => firstCond(lookup(l))
        case (s: AbstractIfStatement) => start
        case _ => throw new IllegalStateException("firstCond: Could not match statements list: " + statements)
      }
    }
  }

  def firstState: AbstractState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    AbstractState(this, 0, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)), Set.empty)
  }
  
  case class Statics(labelTable: Map[Label, Int], statementTable: Map[Int, List[AbstractStatement]], lastLineNumber: Int)

  val statements = s
  val tables = generateTables(statements)
  val lookup = tables.labelTable
  val statementTable = tables.statementTable
  val lastLineNumber = tables.lastLineNumber
  val allStatementLists = {
    def allSuffixes(suffixes: Set[List[AbstractStatement]], lst: List[AbstractStatement]): Set[List[AbstractStatement]] = {
      val withThis = suffixes | Set(lst)
      if (lst.isEmpty) {
        withThis
      } else {
        allSuffixes(withThis, lst.tail)
      }
    }
    allSuffixes(Set.empty, statements)
  }

  override def equals(obj: Any): Boolean = obj match {
    case p: AbstractProgram => p.statements equals statements
    case _ => false
  }

  /*private def generateTables(statements: List[AbstractStatement]): Statics = {
    def innerGenerateTables(statements: List[AbstractStatement], labelsSoFar: Map[Label, List[AbstractStatement]], conditionalsSoFar: Map[Int, List[AbstractStatement]]): Statics = {
      if (statements.isEmpty)
        Statics(labelsSoFar, conditionalsSoFar, 0)
      else {
        val statement = statements.head
        statement match {
          case AbstractLabelStatement(ln, l) => innerGenerateTables(statements.tail, labelsSoFar.+((l, statements)), conditionalsSoFar)
          case AbstractIfStatement(ln, cond, l) => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar.+((ln, statements)))
          case _ => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar)
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty)
  }*/
  
  private def generateTables(statements: List[AbstractStatement]): Statics = {
    def innerGenerateTables(statements: List[AbstractStatement], labelTable: Map[Label, Int], statementTable: Map[Int, List[AbstractStatement]], ln: Int): Statics = {
      if (statements.isEmpty)
        Statics(labelTable, statementTable, ln)
      else {
        val statement = statements.head
        val next = statements.tail
        statement match {
          case AbstractLabelStatement(ln, l) => innerGenerateTables(next, labelTable + Pair(l, ln), statementTable + Pair(ln, statements), ln + 1)
          case AbstractIfStatement(ln, cond, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case AbstractGotoStatement(ln, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case AbstractAssignmentStatement(ln, v, e) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statements), ln + 1)
          case _ => scala.sys.error("generateTables: unknown Statement type")
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty, 0)
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
          case (s: AbstractLabelStatement) => innerPath(start + 1, withStart)
          case AbstractGotoStatement(tl, l) => innerPath(lookup(l), withStart)
          case (s: AbstractAssignmentStatement) => innerPath(start + 1, withStart)
          case (s: AbstractIfStatement) => withStart
          case _ => throw new IllegalStateException("path: unknown statement type")
        }
      }
    }
    innerPath(start, Set.empty)
  }

  def mustReach(s: Int, seen: Set[Int] = Set.empty): Set[Int] = {
    if (seen contains s) {
//      System.err.println("warning: loop. Termination leaks are possible.")
      Set.empty
    } else {
      if (s == lastLineNumber) Set(s) else {
        val nextSeen = seen + s
        statementTable(s).head match {
          case as: AbstractAssignmentStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case ls: AbstractLabelStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case AbstractGotoStatement(ln, l) => mustReach(lookup(l), nextSeen) + lookup(l)
          case AbstractIfStatement(ln, cond, l) => mustReach(s + 1, nextSeen) & mustReach(lookup(l), nextSeen)
        }
      }
    }
  }

  def influence(s: Int): Set[Int] = {
    val must_reach = mustReach(s)
    def innerInfluence(queue: List[Int], seenSources: Set[Int]): Set[Int] = {
      if (queue isEmpty) {
        seenSources
      } else {
        val succs = successors(queue.head)
        if (!((succs.filter((successor) => s == successor)).isEmpty)) {
            System.err.println("warning: loop to sensitive conditional; termination leaks are possible")
        }
        val nextStatements = succs filter ((successor) => !(seenSources contains successor) && successor != s && !(must_reach contains successor))
        innerInfluence(queue.tail ++ nextStatements, seenSources ++ nextStatements)
      }
    }
    statementTable(s).head match {
      case i: AbstractIfStatement => innerInfluence(List(s), Set.empty)
      case _ => Set.empty
    }
  }
}

object AbstractProgramFactory {
  def empty: AbstractProgram = new AbstractProgram(List.empty)
}