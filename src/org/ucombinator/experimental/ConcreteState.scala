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

case class ConcreteState(program: ConcreteProgram, statement: Int, env: Map[Variable, Value], taintedVars: Set[Variable], contextTaint: Set[Int]) {

  override def toString = program.statementTable(statement).toString

  def next: ConcreteState = {
    if (isEnd) {
      scala.sys.error("next: should be unreachable")
    } else {
      val ctPrime = contextTaint.filter((source) => !program.mustReach(source).contains(statement))
      program.statementTable(statement) match {
        case LabelStatement(id, l) => ConcreteState(program, statement + 1, env, taintedVars, ctPrime)
        case GotoStatement(id, l) => ConcreteState(program, program.lookup(l), env, taintedVars, ctPrime)

        case AssignmentStatement(id, v, e) => {
          val envPrime = env + Pair(v, program.eval(e, env))
          val tPrime = if (program.tainted(e, taintedVars) || !(contextTaint.isEmpty)) taintedVars + v else taintedVars - v
          ConcreteState(program, statement + 1, envPrime, tPrime, ctPrime)
        }

        case IfStatement(id, e, l) => {
          val sPrime = if (program.eval(e, env) == Value(0)) {
            statement + 1
          } else {
            program.lookup(l)
          }
          val ctPrimePrime = if (program.tainted(e, taintedVars)) {
            ctPrime + id
          } else {
            ctPrime
          }
          ConcreteState(program, sPrime, env, taintedVars, ctPrimePrime)
        }

        case _ => throw new IllegalStateException("next: unknown statement: " + program.statementTable(statement))
      }
    }
  }

  def isEnd: Boolean = statement == program.lastLineNumber
}