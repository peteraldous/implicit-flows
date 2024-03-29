/*
    Implicit Flows: a prototype taint tracking system for implicit flows
    Copyright (C) 2013   Petey Aldous <petey.aldous@utah.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

package org.ucombinator.experimental

class AbstractProgram(s: List[AbstractStatement]) {

  def tainted(e: AbstractExpression, t: Set[AbstractVariable]): Boolean = e match {
    case AbstractAddition(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case AbstractMultiplication(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case AbstractComparison(lhs, rhs) => tainted(lhs, t) || tainted(rhs, t)
    case v: AbstractValue => false
    case v: AbstractVariable => t contains v
    case _ => throw new IllegalStateException("tainted: unknown expression: " + e)
  }

  def successors(ln: Int): Set[Int] = {
    if (ln == lastLineNumber) Set.empty else statementTable(ln) match {
      case l: AbstractLabelStatement => Set(ln + 1)
      case a: AbstractAssignmentStatement => Set(ln + 1)
      case AbstractGotoStatement(ln, l) => Set(lookup(l))
      case AbstractIfStatement(ln, e, l) => Set(lookup(l), ln + 1)
      case _ => scala.sys.error("successors: unknown statement type")
    }
  }

  def abstractAdd(lhs: AbstractValue, rhs: AbstractValue): AbstractValue = {
    (lhs, rhs) match {
      // nzp is bottom
      case (`nzp`, _) => nzp // 7 cases - 7 total
      case (_, `nzp`) => nzp // 6 cases - 13 total
      // zero doesn't change the case
      case (`z`, s) => s // 5 cases - 19 total
      case (s, `z`) => s // 5 cases - 24 total
      // With n on one side and p in the other, anything is possible
      case (lhs, rhs) if ((AbstractValues.positive.contains(rhs) && AbstractValues.negative.contains(lhs)) ||
        (AbstractValues.negative.contains(rhs) && AbstractValues.positive.contains(lhs))) => nzp
      // 17 cases - 41 total
      case (`p`, `p`) => p
      case (`n`, `n`) => n
      case (`zp`, `zp`) => zp
      case (`nz`, `nz`) => nz
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
      case (`nzp`, _) => nzp // 5 cases  - 29 total
      case (_, `nzp`) => nzp // 4 cases  - 33 total
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

  def firstState: AbstractState = {
    // I should probably make this configurable, but x has the value of 2 and is tainted
    AbstractState(this, 0, Map(Pair(AbstractVariable("x"), nzp)), Set(AbstractVariable("x")), Set.empty)
  }

  case class Statics(labelTable: Map[Label, Int], statementTable: Map[Int, AbstractStatement], lastLineNumber: Int)

  val statements = s
  val statics = generateTables(statements)
  val lookup = statics.labelTable
  val statementTable = statics.statementTable
  val lastLineNumber = statics.lastLineNumber
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

  private def generateTables(statements: List[AbstractStatement]): Statics = {
    def innerGenerateTables(statements: List[AbstractStatement], labelTable: Map[Label, Int], statementTable: Map[Int, AbstractStatement], ln: Int): Statics = {
      if (statements.isEmpty)
        Statics(labelTable, statementTable, ln)
      else {
        val statement = statements.head
        val next = statements.tail
        statement match {
          case AbstractLabelStatement(ln, l) => innerGenerateTables(next, labelTable + Pair(l, ln), statementTable + Pair(ln, statement), ln + 1)
          case AbstractIfStatement(ln, cond, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case AbstractGotoStatement(ln, l) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case AbstractAssignmentStatement(ln, v, e) => innerGenerateTables(next, labelTable, statementTable + Pair(ln, statement), ln + 1)
          case _ => scala.sys.error("generateTables: unknown Statement type")
        }
      }
    }
    innerGenerateTables(statements, Map.empty, Map.empty, 0)
  }

  def mustReach(s: Int, seen: Set[Int] = Set.empty): Set[Int] = {
    if (seen contains s) {
      //      System.err.println("warning: loop. Termination leaks are possible.")
      Set.empty
    } else {
      if (s == lastLineNumber) Set.empty else {
        val nextSeen = seen + s
        statementTable(s) match {
          case as: AbstractAssignmentStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case ls: AbstractLabelStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case AbstractGotoStatement(ln, l) => mustReach(lookup(l), nextSeen) + lookup(l)
          case AbstractIfStatement(ln, cond, l) => mustReach(s + 1, nextSeen) & mustReach(lookup(l), nextSeen)
        }
      }
    }
  }
}

object AbstractProgramFactory {
  def empty: AbstractProgram = new AbstractProgram(List.empty)
  def parse(code: String): AbstractProgram = new AbstractProgram(ToyParser.applyStmts(code) map { _.abstractMe })
}