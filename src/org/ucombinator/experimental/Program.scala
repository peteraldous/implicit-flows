package org.ucombinator.experimental

import java.io.BufferedReader
import java.io.InputStreamReader

object Analyzer extends App {

  def setup(sourceCode: String): State = {
    new Program(ToyParser.applyStmts(sourceCode, 0)).firstState
  }

  def analyze(sourceCode: String): Unit = {
    val firstState = setup(sourceCode)
    printGraph(firstState, explore(firstState, Map.empty))
  }

  def explore(state: State, successorGraph: Map[State, State]): Map[State, State] = {
    if (state.isEnd) successorGraph else {
      val next = state.next
      explore(next, successorGraph + Pair(state, next))
    }
  }

  def printGraph(initial: State, graph: Map[State, State]): Unit = {
    def innerPrintGraph(currentState: State): Unit = {
      if (!currentState.isEnd) {
        println(currentState + " -> " + currentState.next)
        innerPrintGraph(currentState.next)
      }
    }
    innerPrintGraph(initial)
  }

  def finalState(initial: State, graph: Map[State, State]): State = {
    if (graph isDefinedAt initial) {
      finalState(graph(initial), graph)
    } else {
      initial
    }
  }

  val sourceCodeReader = new BufferedReader(new InputStreamReader(System.in))
  val sourceCodeBuilder = new StringBuilder()
  def readALine(reader: BufferedReader, builder: StringBuilder): Boolean = {
    val line = reader.readLine()
    if (line != null) {
      builder.append(line)
      true
    } else {
      false
    }
  }
  while (readALine(sourceCodeReader, sourceCodeBuilder)) {}

  analyze(sourceCodeBuilder.mkString)

}

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

  def firstState: State = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    new State(this)(statements, Map(Pair(Variable("x"), Value(2))), Map(Pair(Variable("x"), true)), Set.empty)
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

  def hcd(sources: Set[List[Statement]]): List[Statement] = {
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

  def influence(s: List[Statement]): Set[List[Statement]] = {
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

class State(program: Program)(s: List[Statement], p: Map[Variable, Value], t: Map[Variable, Boolean], ct: Set[List[Statement]]) {
  val statements = s
  val env = p
  val taintedVars = t
  val contextTaint = ct

  override def toString = "(" + statements + ", " + env + ", " + taintedVars + ", " + contextTaint + ")"

  def next: State = {
    if (s.isEmpty) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = ct.filter((source) => program.influence(source).contains(s))
      s.head match {
        case LabelStatement(id, l) => new State(program)(s.tail, p, t, ctPrime)
        case GotoStatement(id, l) => new State(program)(program.lookup(l), p, t, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val pPrime = p + Pair(v, program.eval(e, p))
          val tPrime = t + Pair(v, program.tainted(e, t) || !(ct.isEmpty))
          new State(program)(s.tail, pPrime, tPrime, ctPrime)
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
          new State(program)(sPrime, p, t, ctPrimePrime)
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + s.head)
      }
    }
  }

  def isEnd: Boolean = s.isEmpty
}