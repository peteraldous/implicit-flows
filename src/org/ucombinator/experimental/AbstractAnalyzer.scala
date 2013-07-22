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

    // TODO consider printing in dot format
    def printGraph: Unit = {
      def innerPrintGraph(currentState: AbstractState, seen: Set[AbstractState] = Set.empty): Unit = {
        if ((successorGraph isDefinedAt currentState) && !(seen contains currentState)) {
          for (state <- successorGraph(currentState)) {
            println(currentState + " -> " + state)
            innerPrintGraph(state, seen + currentState)
          }
        }
      }
      innerPrintGraph(initialState)
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