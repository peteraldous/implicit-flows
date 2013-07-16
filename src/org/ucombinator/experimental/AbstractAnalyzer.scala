package org.ucombinator.experimental

import java.io.BufferedReader
import java.io.InputStreamReader

object AbstractAnalyzer extends App {

  def setup(sourceCode: String): AbstractState = {
    new AbstractProgram(ToyParser.applyStmts(sourceCode, 0) map { (stmt) => stmt.abstractMe }).firstState
  }

  def analyze(sourceCode: String): Unit = {
    val firstState = setup(sourceCode)
    printGraph(firstState, explore(List(firstState)))
  }

  def explore(queue: List[AbstractState], successorGraph: Map[AbstractState, Set[AbstractState]] = Map.empty): Map[AbstractState, Set[AbstractState]] = {
    if (queue.isEmpty) {
      successorGraph
    } else {
      val state = queue.head
      if (state.isEnd || (successorGraph.keys.exists((st) => st equals state))) explore(queue.tail, successorGraph) else {
        val next = state.next
        val newQueue = queue ++ next
        val newGraph = successorGraph + Pair(state, if (successorGraph isDefinedAt state) successorGraph(state) | next else next) 
        explore(newQueue, newGraph)
      }
    }
  }

  def printGraph(initial: AbstractState, graph: Map[AbstractState, Set[AbstractState]]): Unit = {
    def innerPrintGraph(currentState: AbstractState): Unit = {
      if (!currentState.isEnd) {
        for (state <- currentState.next) {
          println(currentState + " -> " + state)
          innerPrintGraph(state)
        }
      }
    }
    innerPrintGraph(initial)
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