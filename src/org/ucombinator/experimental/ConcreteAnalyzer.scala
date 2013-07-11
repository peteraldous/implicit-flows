package org.ucombinator.experimental

import java.io.BufferedReader
import java.io.InputStreamReader

object ConcreteAnalyzer extends App {

  def setup(sourceCode: String): ConcreteState = {
    new Program(ToyParser.applyStmts(sourceCode, 0)).firstState
  }

  def analyze(sourceCode: String): Unit = {
    val firstState = setup(sourceCode)
    printGraph(firstState, explore(firstState, Map.empty))
  }

  def explore(state: ConcreteState, successorGraph: Map[ConcreteState, ConcreteState]): Map[ConcreteState, ConcreteState] = {
    if (state.isEnd) successorGraph else {
      val next = state.next
      explore(next, successorGraph + Pair(state, next))
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