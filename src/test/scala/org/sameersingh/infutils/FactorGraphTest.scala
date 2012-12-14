package org.sameersingh.infutils

import org.junit._
import Assert._
import java.io.{PrintWriter, StringReader}

/**
 * @author sameer
 * @date 10/30/12
 */
@Test
class FactorGraphTest {

  @Test
  def testSimpleGraph(): Unit = {
    val v1 = new Var {
      def numVals = 2

      def id = 1
    }
    val v2 = new Var {
      def numVals = 3

      def id = 2
    }

    var fscores = Seq(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
    val f = new Fact {
      def variables = Seq(v1, v2)

      def scores = fscores
    }
    var i = 0
    for (a <- f.valuesIterator) {
      print(v1 + ":" + a(v1) + "\t")
      print(v2 + ":" + a(v2) + "\t")
      print("fscore: " + f.assignmentScore(a) + "\t")
      print("score: " + fscores(i) + "\n")
      assertEquals(fscores(i), f.assignmentScore(a), 1.0e-9)
      i += 1
    }

  }

  @Test
  def testReadTutorialSimplified(): Unit = {
    val exampleString =
      "MARKOV" + "\n" +
            "3" + "\n" +
            "2 2 3" + "\n" +
            "2" + "\n" +
            "2 0 1" + "\n" +
            "3 0 1 2" + "\n" +
            "4 4.000 2.400 1.000 0.000" + "\n" +
            "12 2.2500 3.2500 3.7500 0.0000 0.0000 10.0000 1.8750 4.0000 3.3330 2.0000 2.0000 3.4000" + "\n"
    val (vs, fs) = UAIReader.read(new StringReader(exampleString))
    for (v <- vs) println(v)
    println("---")
    for (f <- fs) {
      for (a <- f.valuesIterator) {
        for (nv <- f.variables)
          print(nv + ":" + a(nv) + "\t")
        println("score :" + f.assignmentScore(a))
      }
      println("---")
    }
  }

  @Test
  def testReadTutorialSpaced(): Unit = {
    val exampleString =
      "MARKOV" + "\n" +
            "3" + "\n" +
            "2 2 3" + "\n" +
            "2" + "\n" +
            "2 0 1" + "\n" +
            "3 0 1 2" + "\n" +
            "4 \n" +
            "4.000 2.400 \n" +
            "1.000 0.000 \n" +
            "\n" +
            "12 \n" +
            "2.2500 3.2500 3.7500 \n" +
            "0.0000 0.0000 10.0000 \n" +
            "1.8750 4.0000 3.3330 \n" +
            "2.0000 2.0000 3.4000\n"
    val (vs, fs) = UAIReader.read(new StringReader(exampleString))
    for (v <- vs) println(v)
    println("---")
    for (f <- fs) {
      for (a <- f.valuesIterator) {
        for (nv <- f.variables)
          print(nv + ":" + a(nv) + "\t")
        println("score :" + f.assignmentScore(a))
      }
      println("---")
    }
    //DOTWriter.toString(new PrintWriter("test.dot"), vs, fs)
  }
}
