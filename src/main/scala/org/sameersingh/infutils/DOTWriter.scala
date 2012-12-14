package org.sameersingh.infutils

import java.io.{PrintWriter, Writer}
import cc.factorie.{Factor, Variable}

/**
 * @author sameer
 */
object DOTWriter {

  def toString[V <: Var, F <: Fact](writer: PrintWriter, vars: Seq[V], factors: Seq[F]) : Unit = {
    toString(writer, vars, factors, (v:V) => v.id.toString, (f:F) => "")
  }

  def toString[V <: Variable, F <: Factor](writer: PrintWriter, vars: Seq[V], factors: Seq[F], vsuff: (V)=> String, fsuff: (F)=> String): Unit = {
    // header
    writer.println("graph fg {")
    // nodes
    writer.println("  node [shape=circle, color=blue]")
    for (v <- vars) {
      writer.println("  v%s " format(vsuff(v)))
    }
    // factors (and edges)
    writer.println("  node [shape=square, color=black]")
    var fid = 0
    for (f <- factors) {
      //writer.println("  f%d%s " format(fid, fsuff(f)))
      for(nv <- f.variables)
        writer.println("  f%d%s -- v%s" format(fid, fsuff(f), vsuff(nv.asInstanceOf[V])))
      fid += 1
    }
    // footer
    writer.println("}")
    writer.flush()
  }
}
