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

import java.io.StringWriter
import java.util
import scala.collection.mutable

class RTL2GraphSpec extends AnyFreeSpec with CompilerTest {


  def printGraph[N, E](graph: Graph[N, E]): Unit = {
    val exporter = new DOTExporter[N, E]()
    exporter.setVertexAttributeProvider((v) => {
      val map = new util.HashMap[String, Attribute]()
      map.put("label", DefaultAttribute.createAttribute(v.toString))
      map
    })
    val writer = new StringWriter()
    exporter.exportGraph(graph, writer)
    println(writer.toString)
  }

  "rtl2graph with adder" in {
    class Adder extends Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Input(UInt(8.W))
        val c = Output(UInt(9.W))
      })
      io.c := io.a +& io.b
    }

    val (_, annos) = compile(new Adder(), "low", List(RunFirrtlTransformAnnotation(ToGraphPass)))
    val graph = annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
    val expectedGraph = new DefaultDirectedGraph[NodeType, DefaultEdge](classOf[DefaultEdge])
    val io_a = PrimaryInput("io_a")
    val io_b = PrimaryInput("io_b")
    val io_c = PrimaryOutput("io_c")
    val _io_c_T = Node("_io_c_T")
    val adder = PrimOp(firrtl.PrimOps.Add)
    Seq(io_a, io_b, io_c, _io_c_T, adder).foreach { n => expectedGraph.addVertex(n) }
    expectedGraph.addEdge(io_a, adder)
    expectedGraph.addEdge(io_b, adder)
    expectedGraph.addEdge(adder, _io_c_T)
    expectedGraph.addEdge(_io_c_T, io_c)
    printGraph(expectedGraph)

    assert(graph.toString == expectedGraph.toString)
    // assert(graph == expectedGraph) // TODO: this fails for some reason
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
    val (str, annos) = compile(new Reg(), "low", List(RunFirrtlTransformAnnotation(ToGraphPass)))
    val graph = annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
    printGraph(graph)
  }
}
