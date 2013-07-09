package org.ucombinator.experimental.test

abstract class Tester extends App {
  var passed = 0
  var run = 0
  def test(result: Boolean, tag: String): Unit = {
    run += 1
    if (result) {
      passed += 1
    } else {
      println("TEST FAILED: " + tag)
    }
  }

  def tests: Unit

  tests
  println(passed + " of " + run + " tests passed")
}