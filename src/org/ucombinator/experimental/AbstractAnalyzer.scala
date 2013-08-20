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

object AbstractAnalyzer extends App {

  class Result(first: AbstractState, last: Set[AbstractState] = Set.empty, graph: Map[AbstractState, Set[AbstractState]] = Map.empty) {
    val initialState = first
    val finalStates = last
    val successorGraph = graph

    def +(state: AbstractState): Result = {
      new Result(first, last + state, graph)
    }

    def +(pair: Pair[AbstractState, Set[AbstractState]]): Result = {
      new Result(first, last, graph + pair)
    }

    def printGraph: Unit = {
      println("digraph successorGraph {")
      def innerPrintGraph(currentState: AbstractState, seen: Set[AbstractState] = Set.empty): Unit = {
        if ((successorGraph isDefinedAt currentState) && !(seen contains currentState)) {
          for (state <- successorGraph(currentState)) {
            println(currentState + " -> " + state)
            innerPrintGraph(state, seen + currentState)
          }
        }
      }
      innerPrintGraph(initialState)
      println("}")
    }
  }

  def setup(sourceCode: String): AbstractState = {
    new AbstractProgram(ToyParser.applyStmts(sourceCode, 0) map { _.abstractMe }).firstState
  }

  def analyze(sourceCode: String): Result = {
    val firstState = setup(sourceCode)
    explore(firstState)
  }

  def explore(state: AbstractState): Result = {
    explore(List(state), new Result(state))
  }

  def explore(queue: List[AbstractState], intermediateResult: Result): Result = {
    if (queue.isEmpty) {
      intermediateResult
    } else {
      val state = queue.head
      if (state.isEnd) {
        explore(queue.tail, intermediateResult + state)
      } else {
        if (intermediateResult.successorGraph.keys.exists((st) => st equals state)) explore(queue.tail, intermediateResult) else {
          val next = state.next
          val newQueue = queue ++ next
          val graphUpdate = Pair(state, if (intermediateResult.successorGraph isDefinedAt state) intermediateResult.successorGraph(state) | next else next)
          explore(newQueue, intermediateResult + graphUpdate)
        }
      }
    }
  }

  analyze(Source.fromInputStream(System.in).getLines.mkString)

}