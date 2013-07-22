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

  def successors(statements: List[AbstractStatement]): Set[List[AbstractStatement]] = statements match {
    case AbstractGotoStatement(tl, l) :: rest => Set(lookup(l))
    case s :: Nil => Set.empty
    case (s: AbstractLabelStatement) :: rest => Set(rest)
    case (s: AbstractAssignmentStatement) :: rest => Set(rest)
    case AbstractIfStatement(id, c, l) :: rest => Set(rest, lookup(l))
    case _ => throw new IllegalStateException("successors: Could not match statement list: " + statements)
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

  def firstCond(statements: List[AbstractStatement], end: List[AbstractStatement]): List[AbstractStatement] = statements match {
    case Nil => List.empty
    case `end` => List.empty
    case (_: AbstractLabelStatement) :: rest => firstCond(rest, end)
    case (_: AbstractAssignmentStatement) :: rest => firstCond(rest, end)
    case AbstractGotoStatement(tl, l) :: rest => firstCond(lookup(l), end)
    case (s: AbstractIfStatement) :: rest => s :: rest
    case _ => throw new IllegalStateException("firstCond: Could not match statements list: " + statements)
  }

  def firstState: AbstractState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    new AbstractState(this)(statements, Map(Pair(AbstractVariable("x"), p)), Map(Pair(AbstractVariable("x"), true)), Set.empty)
  }

  val statements = s
  val tables = generateTables(statements)
  val lookup = tables._1
  val conditionals = tables._2
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

  private def generateTables(statements: List[AbstractStatement]): Pair[Map[Label, List[AbstractStatement]], Map[Int, List[AbstractStatement]]] = {
    def innerGenerateTables(statements: List[AbstractStatement], labelsSoFar: Map[Label, List[AbstractStatement]], conditionalsSoFar: Map[Int, List[AbstractStatement]]): Pair[Map[Label, List[AbstractStatement]], Map[Int, List[AbstractStatement]]] = {
      if (statements.isEmpty)
        (labelsSoFar, conditionalsSoFar)
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
  }

  def descendants(s: List[AbstractStatement]): Set[List[AbstractStatement]] = {
    val succs = successors(s)
    (succs map descendants).fold(succs)((set1, set2) => set1 | set2)
  }

  def highestCommonDescendant(sources: Set[List[AbstractStatement]]): List[AbstractStatement] = {
    val commonDescendants = (sources map descendants).foldLeft(allStatementLists)((set1, set2) => set1 & set2)
    def firstSuffixMatch(lst: List[AbstractStatement]): Option[List[AbstractStatement]] = {
      if (commonDescendants.contains(lst)) {
        Some(lst)
      } else {
        if (lst.isEmpty) {
          None
        } else {
          lst.head match {
            case AbstractGotoStatement(ln, l) => firstSuffixMatch(lookup(l))
            case AbstractIfStatement(ln, cond, l) => {
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
  def path(end: List[AbstractStatement])(start: List[AbstractStatement]): List[List[AbstractStatement]] = {
    def innerPath(start: List[AbstractStatement], soFar: List[List[AbstractStatement]]): List[List[AbstractStatement]] = {
      if (start.isEmpty)
        throw new IllegalStateException("path: No more statements and the end has not been reached")
      else {
        start match {
          case `end` => soFar
          case (s: AbstractLabelStatement) :: rest => innerPath(rest, start :: soFar)
          case AbstractGotoStatement(tl, l) :: rest => innerPath(lookup(l), start :: soFar)
          case (s: AbstractAssignmentStatement) :: rest => innerPath(rest, start :: soFar)
          case (s: AbstractIfStatement) :: rest => throw new IllegalStateException("path: Conditionals are not permitted")
          case _ => throw new IllegalStateException("path: unknown statement type")
        }
      }
    }
    innerPath(start, Nil)
  }

  def influence(s: List[AbstractStatement]): Set[List[AbstractStatement]] = {
    def innerInfl(sources: Set[List[AbstractStatement]], soFar: Set[List[AbstractStatement]]): Set[List[AbstractStatement]] = {
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
      case i: AbstractIfStatement => innerInfl(successors(s), Set.empty)
      case _ => Set.empty
    }
  }
}

object AbstractProgramFactory {
  def empty: AbstractProgram = new AbstractProgram(List.empty)
}