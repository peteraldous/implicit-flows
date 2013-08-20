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

import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source

object ConcreteAnalyzer extends App {

  class Result(init: ConcreteState, last: ConcreteState, graph: Map[ConcreteState, ConcreteState] = Map.empty) {
    val initialState = init
    val finalState = last
    val successorGraph = graph

    def updateFinal(state: ConcreteState): Result = new Result(initialState, state, successorGraph)
    def +(pair: Pair[ConcreteState, ConcreteState]): Result = new Result(initialState, finalState, successorGraph + pair)

    def printGraph: Unit = {
      println("digraph successorGraph {")
      def innerPrintGraph(currentState: ConcreteState, seen: Set[ConcreteState] = Set.empty): Unit = {
        if (!(seen contains currentState) && (successorGraph isDefinedAt currentState)) {
          val next = successorGraph(currentState)
          println(currentState + " -> " + next)
          innerPrintGraph(next, seen + currentState)
        }
      }
      innerPrintGraph(initialState)
      println("}")
    }
  }

  def setup(sourceCode: String): ConcreteState = {
    new ConcreteProgram(ToyParser.applyStmts(sourceCode, 0)).firstState
  }

  def analyze(sourceCode: String): Result = {
    val firstState = setup(sourceCode)
    startExploring(firstState)
  }

  def startExploring(state: ConcreteState): Result = {
    explore(state, new Result(state, state))
  }

  def explore(state: ConcreteState, intermediateResult: Result): Result = {
    if (state.isEnd) intermediateResult updateFinal state else {
      val next = state.next
      explore(next, intermediateResult + Pair(state, next))
    }
  }

  analyze(Source.fromInputStream(System.in).getLines.mkString)

}