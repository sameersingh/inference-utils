package org.sameersingh.infutils

import collection.mutable.ArrayBuffer
import java.io.{PrintWriter, InputStream, FileReader}
import java.util.Scanner

/**
 * @author sameer
 */
object UAIReader {

  def read(filename: String): (Seq[Var], Seq[Fact]) = read(filename, (_id: Int, _vals: Int) => new Var {
    def id = _id

    def numVals = _vals
  }, (_vars: Seq[Var], _scores: Seq[Double]) => new Fact {
    def variables = _vars

    def scores = _scores
  })

  // Read from a file
  def read(filename: String, newVar: (Int, Int) => Var, newFact: (Seq[Var], Seq[Double]) => Fact): (Seq[Var], Seq[Fact]) = read(new FileReader(filename), newVar, newFact)

  // Read from an arbitrary @Readable object
  def read(readable: Readable, newVar: (Int, Int) => Var = (_id: Int, _vals: Int) => new Var {
    def id = _id

    def numVals = _vals
  }, newFact: (Seq[Var], Seq[Double]) => Fact = (_vars: Seq[Var], _scores: Seq[Double]) => new Fact {
    def variables = _vars

    def scores = _scores
  }): (Seq[Var], Seq[Fact]) = {
    val vars = new ArrayBuffer[Var]
    val facts = new ArrayBuffer[Fact]
    val scanner = new Scanner(readable)
    // header
    var head = scanner.nextLine()
    assert(head.trim.toUpperCase() == "MARKOV")
    // preamble
    val numVars = scanner.nextLine().trim.toInt
    val sizes = scanner.nextLine().split("\\s+").map(_.toInt)
    for (id <- 0 until numVars) {
      vars += newVar(id, sizes(id))
    }
    val numFacts = scanner.nextLine().trim.toInt
    val factNeighs = new ArrayBuffer[Seq[Int]]
    for (fid <- 0 until numFacts) {
      val fnums = scanner.nextLine().split("\\s+").map(_.toInt)
      val numNeighs = fnums.head
      val neighs = fnums.drop(1)
      assert(neighs.length == numNeighs)
      factNeighs += neighs
    }
    // function tables
    for (fid <- 0 until numFacts) {
      val neighs = factNeighs(fid).map(id => vars(id)).toSeq
      val totalDomainSize = neighs.map(_.domain.size).foldLeft(1)(_ * _)
      val scores = new ArrayBuffer[Double]
      //val fstrings = scanner.nextLine().split("\\s+")
      //assert(fstrings.head.trim.toInt == totalDomainSize)
      assert(totalDomainSize == scanner.nextInt())
      //assert(fstrings.length - 1 == totalDomainSize)
      (0 until totalDomainSize).foreach(i => scores += scanner.nextDouble()) //fstrings(i + 1).trim.toDouble)
      facts += newFact(neighs, scores)
    }
    scanner.close()
    (vars, facts)
  }
}
