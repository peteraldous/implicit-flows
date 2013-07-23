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
    new Program(ToyParser.applyStmts(sourceCode, 0)).firstState
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