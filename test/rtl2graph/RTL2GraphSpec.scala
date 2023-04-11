package rtl2graph

import org.scalatest.freespec.AnyFreeSpec
import chisel3._
import chisel3.util._
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.{Graph, io}
import org.jgrapht.nio.{Attribute, DefaultAttribute}
import org.jgrapht.nio.dot.DOTExporter
import rtl2graph.ToGraphPass.GraphAnnotation
import rtl2graph.ToGraphPass._

import java.io.{File, PrintWriter, StringWriter}
import java.util
import scala.collection.mutable
import scala.io.Source

class RTL2GraphSpec extends AnyFreeSpec with CompilerTest {
  def getChiselGraph[M <: Module](gen: => M): Graph[NodeType, EdgeType] = {
    val (firrtl, annos) = compile(gen, "low", List(RunFirrtlTransformAnnotation(ToGraphPass)))
    println(firrtl)
    annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
  }

  def getGraph[N](graph: Graph[N, EdgeType]): String = {
    val exporter = new DOTExporter[N, EdgeType]()
    exporter.setVertexAttributeProvider((v) => {
      val map = new util.HashMap[String, Attribute]()
      map.put("label", DefaultAttribute.createAttribute(v.toString))
      map
    })
    exporter.setEdgeAttributeProvider(e => {
      val map = new util.HashMap[String, Attribute]()
      map.put("label", DefaultAttribute.createAttribute(e.getDOTLabel))
      map
    })
    val writer = new StringWriter()
    exporter.exportGraph(graph, writer)
    writer.toString
  }

  "rtl2graph should work with an adder" in {
    class Adder extends Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Input(UInt(8.W))
        val c = Output(UInt(9.W))
      })
      io.c := io.a +& io.b
    }

    val graph = getChiselGraph(new Adder())
    println(getGraph(graph))
  }

  "rtl2graph should work with a register" in {
    class Reg extends Module {
      val io = IO(new Bundle{
        val d = Input(Bool())
        val q = Output(Bool())
      })
      val r = Reg(Bool())
      r := io.d
      io.q := r
    }

    val graph = getChiselGraph(new Reg())
    println(getGraph(graph))
  }

  "rtl2graph should work with an arbiter" in {
    val graph = getChiselGraph(new Arbiter(UInt(8.W), 4))
    println(getGraph(graph))
  }

  "rtl2graph should work with an queue" in {
    val graph = getChiselGraph(new Queue(UInt(8.W), 4))
//    val (firrtl, annos) = compile(new Queue(UInt(8.W), 4), "low", List())
//    println(firrtl)
    println(getGraph(graph))
  }

  "rtl2graph should work with nested modules" in {
       class Helo extends Module {
         val io = IO(new Bundle{
           val q = Output(Bool())
         })

         val x = Module(new Reg())
         val y = Module(new Reg())
         y.io.d := true.B
         x.io.d := y.io.q
         io.q := x.io.q
       }

       class Reg extends Module {
         val io = IO(new Bundle {
           val d = Input(Bool())
           val q = Output(Bool())
         })
         val r = Reg(Bool())
         r := io.d
         io.q := r
       }

       val graph = getChiselGraph(new Helo())
       println(getGraph(graph))
     }

  "rtl2graph should work with nested modules ansa edition" in {
    class Top extends Module {
      val io = IO(new Bundle {
        val q = Output(Bool())
      })

      val x = Module(new First())
      val y = Module(new Second())

      x.io.d := y.io.q
      y.io.d := x.io.q
      io.q := x.io.d && y.io.d

    }

    class First extends Module {
      val io = IO(new Bundle {
        val d = Input(Bool())
        val q = Output(Bool())
      })
      val r = Reg(Bool())
      r := io.d
      io.q := r
    }

    class Second extends Module {
      val io = IO(new Bundle{
        val d = Input(Bool())
        val q = Output(Bool())
      })
      io.q := io.d
    }

    val graph = getChiselGraph(new Top())
//    val (firrtl, annos) = compile(new Queue(UInt(8.W), 4), "low", List())
//    println(firrtl)
    println(getGraph(graph))
  }

  "rtl2graph should work with rocket" in {
    val firrtlStr = Source.fromFile("rocketchip.fir").mkString
//    println(firrtlStr)
    val (str, annos) = fromFirrtlString(firrtlStr, List(RunFirrtlTransformAnnotation(ToGraphPass)))
    val graph = annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
    val dot = getGraph(graph)
    val pw = new PrintWriter(new File("/Users/njha/Downloads/rocket.dot"))
    pw.write(dot)
    pw.close
  }
}
