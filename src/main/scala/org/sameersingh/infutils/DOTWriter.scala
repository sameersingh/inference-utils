package org.sameersingh.infutils

import java.io.{PrintWriter, Writer}
import cc.factorie.{Model, Factor, Variable}

/**
 * @author sameer
 */
object DOTWriter {

  def toString[V <: Var, F <: Fact](writer: PrintWriter, vars: Iterable[V], factors: Iterable[F]): Unit = {
    toString(writer, vars, factors, (v: V) => v.id.toString, (f: F) => "")
  }

  def toString[V <: Variable, F <: Factor](writer: PrintWriter, vars: Iterable[V], factors: Iterable[F], vsuff: (V) => String, fsuff: (F) => String): Unit = {
    // header
    writer.println("graph fg {")
    // nodes
    writer.println("  node [shape=circle, color=blue]")
    for (v <- vars) {
      writer.println("  v%s " format (vsuff(v)))
    }
    // factors (and edges)
    writer.println("  node [shape=square, color=black]")
    var fid = 0
    for (f <- factors) {
      //writer.println("  f%d%s " format(fid, fsuff(f)))
      for (nv <- f.variables)
        writer.println("  f%d%s -- v%s" format(fid, fsuff(f), vsuff(nv.asInstanceOf[V])))
      fid += 1
    }
    // footer
    writer.println("}")
    writer.flush()
  }

  def writeToFile[V <: Variable](filename: String, vars: Seq[V], model: Model): Unit = toString[V,Factor](new PrintWriter(filename), vars, model.factorsOfClass(vars), _.toString(), _.toString())

  def main(args: Array[String]) = {
    val file = "/Users/sameer/data/uai/samples/network.uai"
    val (vs, fs) = UAIReader.read(file)
    DOTWriter.toString(new PrintWriter(file + ".dot"), vs, fs)
  }
}
