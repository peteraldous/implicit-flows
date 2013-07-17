package org.ucombinator.experimental

import java.io.BufferedReader
import java.io.InputStreamReader

object ConcreteAnalyzer extends App {
  
  class Result(init: ConcreteState, last: ConcreteState, graph: Map[ConcreteState, ConcreteState] = Map.empty) {
    val initialState = init
    val finalState = last
    val successorGraph = graph
    
    def updateFinal(state: ConcreteState): Result = new Result(initialState, state, successorGraph)
    def +(pair: Pair[ConcreteState, ConcreteState]): Result = new Result(initialState, finalState, successorGraph + pair)
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

  def printGraph(initial: ConcreteState, graph: Map[ConcreteState, ConcreteState]): Unit = {
    def innerPrintGraph(currentState: ConcreteState): Unit = {
      if (!currentState.isEnd) {
        println(currentState + " -> " + currentState.next)
        innerPrintGraph(currentState.next)
      }
    }
    innerPrintGraph(initial)
  }

  def finalState(initial: ConcreteState, graph: Map[ConcreteState, ConcreteState]): ConcreteState = {
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