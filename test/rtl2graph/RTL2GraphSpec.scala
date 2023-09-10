package rtl2graph

import org.scalatest.freespec.AnyFreeSpec
import chisel3._
import chisel3.util._
// import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.VerilogEmitter
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.{Graph, io}
import org.jgrapht.nio.{Attribute, DefaultAttribute}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.gml.GmlExporter
import org.jgrapht.nio.gml.GmlExporter.Parameter
import rtl2graph.ToGraphPass.GraphAnnotation
import rtl2graph.ToGraphPass._
// import rtl2graph.ToGraphPassPerModule.GraphAnnotation
// import rtl2graph.ToGraphPassPerModule._

import java.io.{File, PrintWriter, StringWriter}
import java.util
import scala.collection.mutable
import scala.io.Source
import scala.reflect.runtime.universe._

class RTL2GraphSpec extends AnyFreeSpec with CompilerTest {
 
  def getSubmoduleGraphs[M <: Module](gen: => M): Seq[Graph[NodeType, EdgeType]] = {
    val (firrtl, annos) = compile(gen, "low", List(RunFirrtlTransformAnnotation(ToGraphPass)))
    // println(firrtl)
    annos.collect {
      case c: GraphAnnotation => c.graph
    }
  }

  def getSubmoduleGraphsFromFIRRTL(firrtlStr: String): Seq[Graph[NodeType, EdgeType]] = {
    val (firrtl, annos) = fromFirrtlString(firrtlStr, List(RunFirrtlTransformAnnotation(ToGraphPass)))
    // println(firrtl)
    annos.collect {
      case c: GraphAnnotation => c.graph
    }
  }

  def getChiselGraph[M <: Module](gen: => M): Graph[NodeType, EdgeType] = {
    val (firrtl, annos) = compile(gen, "low", List(
      RunFirrtlTransformAnnotation(ToGraphPass), 
      RunFirrtlTransformAnnotation(new VerilogEmitter))
    )
    println(firrtl)
    annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
  }

  def getChiselGraphFromFIRRTL(filename: String): Graph[NodeType, EdgeType] = {
    val firrtlStr = Source.fromFile(filename).mkString
    val (str, annos) = fromFirrtlString(firrtlStr, List(RunFirrtlTransformAnnotation(ToGraphPass)))
    annos.collectFirst {
      case c: GraphAnnotation => c
    }.get.graph
  }

  def getDOTGraph[N](graph: Graph[N, EdgeType]): String = {
    val dot_exporter = new DOTExporter[N, EdgeType]()
    dot_exporter.setVertexAttributeProvider((v) => {
      val map = new util.HashMap[String, Attribute]()
      map.put("label", DefaultAttribute.createAttribute(v.toString))
      map
    })
    dot_exporter.setEdgeAttributeProvider(e => {
      val map = new util.HashMap[String, Attribute]()
      map.put("label", DefaultAttribute.createAttribute(e.getDOTLabel))
      map
    })
    val writer = new StringWriter()
    dot_exporter.exportGraph(graph, writer)
    writer.toString
  }

  def getGMLGraph[N](graph: Graph[N, EdgeType]): String = {
    val exporter = new GmlExporter[N, EdgeType]()
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
    // set exporter parameters: print vertex and edge labels
    exporter.setParameter(Parameter.EXPORT_VERTEX_LABELS, true)
    exporter.setParameter(Parameter.EXPORT_EDGE_LABELS, true)
    val writer = new StringWriter()
    exporter.exportGraph(graph, writer)
    writer.toString
  }

  "adder" in {
    class Adder extends Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Input(UInt(8.W))
        val c = Output(UInt(9.W))
      })
      io.c := io.a +& io.b
    }

    val graph = getChiselGraph(new Adder())

    println(getGMLGraph(graph))
    val gml = getGMLGraph(graph)
    val pw = new PrintWriter(new File("adder.gml"))
    pw.write(gml)
    pw.close
  }

  "rocket" in {
    // val graph = getChiselGraphFromFIRRTL("rocketchip.fir")
    // val gml = getGMLGraph(graph)
    // val pw = new PrintWriter(new File("rocket.gml"))
    // pw.write(gml)
    // pw.close
    val firrtlStr = Source.fromFile("rocketchip.fir").mkString
    val subGraphs = getSubmoduleGraphsFromFIRRTL(firrtlStr)
    for (graph <- subGraphs) {
      val pw = new PrintWriter(new File("rocket.gml"))
      val gml = getGMLGraph(graph)
      println(gml)
      pw.write(gml)
      pw.close
    }
  }

  // "rtl2graph should work with a register" in {
  //   class Reg extends Module {
  //     val io = IO(new Bundle{
  //       val d = Input(Bool())
  //       val q = Output(Bool())
  //     })
  //     val r = Reg(Bool())
  //     r := io.d
  //     io.q := r
  //   }

  //   val graph = getChiselGraph(new Reg())
  //   println(getGMLGraph(graph))
  // }

  // "rtl2graph should work with an arbiter" in {
  //   val graph = getChiselGraph(new Arbiter(UInt(8.W), 4))
  //   println(getGMLGraph(graph))
  // }

  // "rtl2graph should work with an queue" in {
  //   val graph = getChiselGraph(new Queue(UInt(8.W), 4))
  //   val (firrtl, annos) = compile(new Queue(UInt(8.W), 4), "low", List())
  //   println(firrtl)
  //   println(getGMLGraph(graph))
  // }

  "nested" in {
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
      println(getGMLGraph(graph))
      val subGraphs = getSubmoduleGraphs(new Top())
      for (graph <- subGraphs) {
        println(getGMLGraph(graph))
      }
     }

//   "rtl2graph should work with nested modules ansa edition" in {
//     class Top extends Module {
//       val io = IO(new Bundle {
//         val q = Output(Bool())
//       })

//       val x = Module(new First())
//       val y = Module(new Second())

//       x.io.d := y.io.q
//       y.io.d := x.io.q
//       io.q := x.io.d && y.io.d

//     }

//     class First extends Module {
//       val io = IO(new Bundle {
//         val d = Input(Bool())
//         val q = Output(Bool())
//       })
//       val r = Reg(Bool())
//       r := io.d
//       io.q := r
//     }

//     class Second extends Module {
//       val io = IO(new Bundle{
//         val d = Input(Bool())
//         val q = Output(Bool())
//       })
//       io.q := io.d
//     }

//     val graph = getChiselGraph(new Top())
// //    val (firrtl, annos) = compile(new Queue(UInt(8.W), 4), "low", List())
// //    println(firrtl)
//     println(getGraph(graph))
//   }

}
