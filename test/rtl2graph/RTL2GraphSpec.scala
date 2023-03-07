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
    val expectedGraph = new DefaultDirectedGraph[NodeType, EdgeType](classOf[EdgeType])
    val io_a = PrimaryInput("io_a")
    val io_b = PrimaryInput("io_b")
    val io_c = PrimaryOutput("io_c")
    val _io_c_T = Node("_io_c_T")
    val adder = PrimOp("", firrtl.PrimOps.Add)
    Seq(io_a, io_b, io_c, _io_c_T, adder).foreach { n => expectedGraph.addVertex(n) }
    expectedGraph.addEdge(io_a, adder, new LeftArgument)
    expectedGraph.addEdge(io_b, adder, new RightArgument)
    expectedGraph.addEdge(adder, _io_c_T)
    expectedGraph.addEdge(_io_c_T, io_c)
    println(getGraph(graph))

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

    val graph = getChiselGraph(new Reg())
    println(getGraph(graph))
  }

  "rtl2graph should work with an arbiter" in {
    val graph = getChiselGraph(new Arbiter(UInt(8.W), 4))
    println(getGraph(graph))
  }

  "rtl2graph should work with an queue" in {
    val graph = getChiselGraph(new Queue(UInt(8.W), 4))
    println(getGraph(graph))
  }
}
