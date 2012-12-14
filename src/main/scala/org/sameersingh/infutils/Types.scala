package org.sameersingh.infutils

import cc.factorie._

/**
 * @author sameer
 */

trait Var extends DiscreteVariable {
  def id: Int

  def numVals: Int

  def domain = new DiscreteDomain(numVals)

  type ContainedVariableType = this.type

  override def toString() = id + "(" + numVals + ")"
}

trait Fact extends Factor {
  thisFactor =>
  def variables: Seq[Var]

  def scores: Seq[Double]

  def numVariables = variables.length

  def variable(index: Int): Var = variables(index)

  def assignmentScore(a: Assignment) = {
    val values = variables.map(v => a.get(v).get.intValue)
    var index = 0
    // TODO check indexing
    for (i <- 0 until numVariables) {
      index = index * variable(i).numVals
      index += values(i)
    }
    scores(index)
  }

  type StatisticsType = Double

  def currentScore = assignmentScore(currentAssignment)

  def currentAssignment = new HashMapAssignment(variables)

  def valuesIterator = new ValuesIterator {
    val totalSize = variables.map(_.domain.size).foldLeft(1)(_ * _)
    val _assignment = new HashMapAssignment()
    variables.foreach(v => _assignment(v) = v.domain(0))
    var _index = 0
    var _hasNext = variables.exists(_.domain.size > 0)

    def next(): Assignment = {
      var tmpIndex = _index
      _index += 1
      for (i <- 0 until numVariables) {
        val v = variable(numVariables - i - 1)
        val ds = v.domain.size
        val value = tmpIndex % ds
        tmpIndex = tmpIndex / ds
        _assignment(v) = v.domain(value)
      }
      _assignment
    }

    def hasNext = _index < totalSize

    def valuesTensor = throw new Error("What?")

    def score = scores(_index)

    def factor = thisFactor
  }
}