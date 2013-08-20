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

case class AbstractState(program: AbstractProgram, statements: Int, env: Map[AbstractVariable, AbstractValue], taintedVars: Set[AbstractVariable], contextTaint: Set[Int]) {

  override def toString = program.statementTable(statements).toString

  def next: Set[AbstractState] = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => !program.mustReach(source).contains(statements))
      program.statementTable(statements) match {
        case AbstractLabelStatement(id, l) => Set(AbstractState(program, statements + 1, env, taintedVars, ctPrime))
        case AbstractGotoStatement(id, l) => Set(AbstractState(program, program.lookup(l), env, taintedVars, ctPrime))

        case AbstractAssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = if (program.tainted(e, taintedVars) || !(contextTaint.isEmpty)) taintedVars + v else taintedVars - v
          Set(AbstractState(program, statements + 1, envPrime, tPrime, ctPrime))
        }

        case AbstractIfStatement(id, e, l) => {
          val condResult = program.eval(e, env)
          val statementListSet = Set(statements)
          val fallThrough = if (AbstractValues.zero.contains(condResult)) Set(statements + 1) else statementListSet.empty
          val jump = if (AbstractValues.positive.contains(condResult)) Set(program.lookup(l)) else statementListSet.empty
          val sPrimes = fallThrough | jump
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + id
          } else {
            ctPrime
          }
          val stateSet = Set(this)
          sPrimes.foldLeft(stateSet.empty)((states, sPrime) => states + AbstractState(program, sPrime, env, taintedVars, ctPrimePrime))
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + program.statementTable(statements))
      }
    }
  }

  def isEnd: Boolean = statements == program.lastLineNumber

}

object AbstractStateFactory {
  def empty: AbstractState = AbstractProgramFactory.empty.firstState
}