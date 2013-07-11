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
      case (`nzp`, _) => nzp
      case (_, `nzp`) => nzp
      // if they match (assuming no overflow), the sign remains the same
      case (s, `s`) => s
      // zero doesn't change the case
      case (`z`, s) => s
      case (s, `z`) => s
      // this catches all cases that have n in one and p in the other
      case (lhs, rhs) if ((rhs.positive && lhs.negative) || (rhs.negative && lhs.positive)) => nzp
      // TODO cases that involve at least one of {nz, zp, np} and possible one of {n, z, p}
    }
  }
  
  def abstractMultiply(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      case (`z`, _) => z
      case (_, `z`) => z
      case (`p`, s) => s
      case (s, `p`) => s
      case (`nzp`, s) => nzp
      case (s, `nzp`) => nzp
      case (`n`, `n`) => p
      case (`n`, `nz`) => zp
      case (`nz`, `n`) => zp
      case (`n`, `zp`) => nz
      case (`zp`, `n`) => nz
      case (`n`, `np`) => np
      case (`np`, `n`) => np
      case (`nz`, `nz`) => zp
      case (`zp`, `zp`) => nz
      case (`np`, `np`) => np
      // TODO cases with nz, np, and zp
    }
  }
  
  def abstractCompare(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      // this is the only case that must be true (positive)
      case (`z`, `z`) => p
      case (lhs, rhs) => if ((lhs.positive && rhs.positive) || (lhs.zero && rhs.zero) || (lhs.negative && rhs.negative)) zp else z
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

  // TODO rename and document, possibly refactoring
  def path(end: List[AbstractStatement])(statements: List[AbstractStatement]): List[List[AbstractStatement]] = {
    def innerPath(statements: List[AbstractStatement], soFar: List[List[AbstractStatement]]): List[List[AbstractStatement]] = {
      if (statements.isEmpty)
        throw new IllegalStateException("path: No more statements and the end has not been reached")
      else {
        statements match {
          case `end` => soFar
          case (s: AbstractLabelStatement) :: rest => innerPath(rest, statements :: soFar)
          case AbstractGotoStatement(tl, l) :: rest => innerPath(lookup(l), statements :: soFar)
          case (s: AbstractAssignmentStatement) :: rest => innerPath(rest, statements :: soFar)
          case (s: AbstractIfStatement) :: rest => throw new IllegalStateException("path: Conditionals are not permitted")
          case _ => throw new IllegalStateException("path: unknown statement type")
        }
      }
    }
    innerPath(statements, Nil)
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

  def hcd(sources: Set[List[AbstractStatement]]): List[AbstractStatement] = {
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

  def influence(s: List[AbstractStatement]): Set[List[AbstractStatement]] = {
    def innerInfl(sources: Set[List[AbstractStatement]], soFar: Set[List[AbstractStatement]]): Set[List[AbstractStatement]] = {
      val sourceHCD = hcd(sources)
      val (clearPaths, condPaths) = sources.partition((source) => firstCond(source, sourceHCD).isEmpty)
      if (condPaths.isEmpty) {
        clearPaths.map(path(sourceHCD)).foldLeft(soFar)((set, list) => set | list.toSet)
      } else {
        def condSuccessors(pathWithCond: List[AbstractStatement]): Set[List[AbstractStatement]] = {
          successors(firstCond(pathWithCond, sourceHCD))
        }
        val newSources = condPaths.foldLeft(clearPaths)((accumulated, condPath) => accumulated | condSuccessors(condPath))
        val pathsToConds = condPaths.map((condPath) => path(firstCond(condPath, sourceHCD))(condPath))
        val statementsInPathsToConds = pathsToConds.foldLeft(soFar)((statements, thisPath) => statements | thisPath.toSet)
        innerInfl(newSources, statementsInPathsToConds)
      }
    }
    s.head match {
      case i: AbstractIfStatement => innerInfl(successors(s), Set.empty)
      case _ => Set.empty
    }
  }
}