package org.ucombinator.experimental

class Label

abstract class Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
case class Comparison(lhs: Expression, rhs: Expression) extends Expression
case class Variable(v: String) extends Expression
case class Value(v: Int) extends Expression

abstract class Statement(tl: Label)
case class LabelStatement(tl: Label, l: Label) extends Statement(tl: Label)
case class AssignmentStatement(tl: Label, v: Variable, e: Expression) extends Statement(tl: Label)
case class GotoStatement(tl: Label, l: Label) extends Statement(tl: Label)
case class IfStatement(tl: Label, condition: Expression, l: Label) extends Statement(tl: Label)
//case object End extends Statement(tl: Label)

object Analyzer extends App {
  val program = new Program(List.empty)
  val firstState = new State(program)(program.statements, Map.empty, Map.empty, Set.empty)
  
  // TODO do a state exploration

  class State(program: Program)(s: List[Statement], p: Map[Variable, Value], t: Map[Variable, Boolean], ct: Set[List[Statement]]) {
    val statement = s.head

    def next: State = {
      val ctPrime = ct.filter((source) => program.infl(source).contains(s))
      s.head match {
        case LabelStatement(id, l) => (s.tail, p, t, ctPrime)
        case GotoStatement(id, l) => (program.lookup(l), p, t, ctPrime)
        case AssignmentStatement(id, v, e) => {
          val pPrime = p + Pair(v, program.eval(e, p))
          val tPrime = t + Pair(v, program.tainted(e, t) || !(ct.isEmpty))
          (s.tail, pPrime, tPrime, ctPrime)
        }
        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, p) == Value(0)) {
            s.tail
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, t)) {
            ctPrime + program.conditionals(id)
          } else {
            ctPrime
          }
          (sPrime, p, t, ctPrimePrime)
        }
        case _ => throw new IllegalStateException("next: unknown statement: " + s.head)
      }
      this
    }
  }

  class Program(s: List[Statement]) {

    def tainted(e: Expression, t: Map[Variable, Boolean]): Boolean = {
      e match {
        case Addition(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
        case Multiplication(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
        case Comparison(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
        case v: Value => false
        case v: Variable => t(v)
        case _ => throw new IllegalStateException("tainted: unknown expression: " + e)
      }
    }

    def successors(statements: List[Statement]): Set[List[Statement]] = {
      statements match {
        case GotoStatement(tl, l) :: rest => Set(lookup(l))
        case s :: Nil => Set.empty
        case (s: LabelStatement) :: rest => Set(rest)
        case (s: AssignmentStatement) :: rest => Set(rest)
        case IfStatement(id, c, l) :: rest => Set(rest, lookup(l))
        case _ => throw new IllegalStateException("successors: Could not match statement list: " + statements)
      }
    }

    def eval(e: Expression, p: Map[Variable, Value]): Value = {
      e match {
        case Addition(lhs, rhs) => new Value(eval(lhs, p).v + eval(rhs, p).v)
        case Multiplication(lhs, rhs) => new Value(eval(lhs, p).v * eval(rhs, p).v)
        case Comparison(lhs, rhs) => new Value(if (eval(lhs, p).v == eval(rhs, p).v) 1 else 0)
        case v: Variable => p(v)
        case v: Value => v
        case _ => throw new IllegalStateException("eval: Could not match expression: " + e)
      }
    }

    def firstCond(statements: List[Statement], end: List[Statement]): List[Statement] = {
      statements match {
        case Nil => List.empty
        case `end` => List.empty
        case (_: LabelStatement) :: rest => firstCond(rest, end)
        case (_: AssignmentStatement) :: rest => firstCond(rest, end)
        case GotoStatement(tl, l) :: rest => firstCond(lookup(l), end)
        case (s: IfStatement) :: rest => s :: rest
        case _ => throw new IllegalStateException("firstCond: Could not match statements list: " + statements)
      }
    }

    // TODO rename and document, possibly refactoring
    def path(end: List[Statement])(statements: List[Statement]): List[List[Statement]] = {
      def innerPath(statements: List[Statement], soFar: List[List[Statement]]): List[List[Statement]] = {
        if (statements.isEmpty)
          throw new IllegalStateException("path: No more statements and the end has not been reached")
        else {
          statements match {
            case `end` => soFar
            case (s: LabelStatement) :: rest => innerPath(rest, statements :: soFar)
            case GotoStatement(tl, l) :: rest => innerPath(lookup(l), statements :: soFar)
            case (s: AssignmentStatement) :: rest => innerPath(rest, statements :: soFar)
            case (s: IfStatement) :: rest => throw new IllegalStateException("path: Conditionals are not permitted")
            case _ => throw new IllegalStateException("path: unknown statement type")
          }
        }
      }
      innerPath(statements, Nil)
    }

    // TODO find a way to get a list of statements into this object
    val statements = s
    val tables = generateTables(statements)
    val lookup = tables._1
    val conditionals = tables._2

    private def generateTables(statements: List[Statement]): Pair[Map[Label, List[Statement]], Map[Label, List[Statement]]] = {
      def innerGenerateTables(statements: List[Statement], labelsSoFar: Map[Label, List[Statement]], conditionalsSoFar: Map[Label, List[Statement]]): Pair[Map[Label, List[Statement]], Map[Label, List[Statement]]] = {
        if (statements.isEmpty)
          (labelsSoFar, conditionalsSoFar)
        else {
          val statement = statements.head
          statement match {
            case LabelStatement(tl, l) => innerGenerateTables(statements.tail, labelsSoFar.+((l, statements)), conditionalsSoFar)
            case IfStatement(tl, cond, l) => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar.+((tl, statements)))
            case _ => innerGenerateTables(statements.tail, labelsSoFar, conditionalsSoFar)
          }
        }
      }
      innerGenerateTables(statements, Map.empty, Map.empty)
    }

    def descendants(s: List[Statement]): Set[List[Statement]] = {
      val succs = successors(s)
      val immediate = for (succ <- succs) yield succ
      val indirect = for (succ <- succs) yield descendants(succ)
      indirect.fold(immediate)((set1, set2) => set1 | set2)
    }

    def hcd(sources: Set[List[Statement]]): List[Statement] = {
      val descendantSets = for (source <- sources) yield descendants(source)
      val aDescendantSet = descendantSets.head
      val commonDescendants = descendantSets.tail.fold(aDescendantSet)((set1, set2) => set1 & set2)
      def firstSuffixMatch(lst: List[Statement]): List[Statement] = {
        if (commonDescendants.contains(lst)) {
          lst
        } else {
          firstSuffixMatch(lst.tail)
        }
      }
      // Find the first statement list in some element of sources that matches and return it
      firstSuffixMatch(sources.head)
    }

    def infl(s: List[Statement]): Set[List[Statement]] = {
      def innerInfl(sources: Set[List[Statement]], soFar: Set[List[Statement]]): Set[List[Statement]] = {
        val sourceHCD = hcd(sources)
        val (clearPaths, condPaths) = sources.partition((source) => firstCond(source, sourceHCD).isEmpty)
        if (condPaths.isEmpty) {
          clearPaths.map(path(sourceHCD)).foldLeft(soFar)((set, list) => set | list.toSet)
        } else {
          def condSuccessors(pathWithCond: List[Statement]): Set[List[Statement]] = {
            successors(firstCond(pathWithCond, sourceHCD))
          }
          val newSources = condPaths.foldLeft(clearPaths)((accumulated, condPath) => accumulated | condSuccessors(condPath))
          val pathsToConds = condPaths.map((condPath) => path(firstCond(condPath, sourceHCD))(condPath))
          val statementsInPathsToConds = pathsToConds.foldLeft(soFar)((statements, thisPath) => statements | thisPath.toSet)
          innerInfl(newSources, statementsInPathsToConds)
        }
      }
      s.head match {
        case i: IfStatement => innerInfl(successors(s), Set.empty)
        case _ => Set.empty
      }
    }
  }

}