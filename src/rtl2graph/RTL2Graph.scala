package rtl2graph

// Compiler Infrastructure
import firrtl.PrimOps._
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Dependency, Unserializable}
import firrtl.transforms.PropagatePresetAnnotations
import firrtl.{CircuitState, DependencyAPIMigration, Transform, Utils}
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
// Firrtl IR classes
import firrtl.ir.{DefModule, Expression, Mux, Statement}
// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable


object ToGraphPass extends Transform with DependencyAPIMigration {
  case class GraphAnnotation(graph: DefaultDirectedGraph[NodeType, DefaultEdge]) extends NoTargetAnnotation with Unserializable

  // run on lowered firrtl
  override def prerequisites = Seq(
    Dependency(firrtl.passes.ExpandWhens),
    Dependency(firrtl.passes.LowerTypes),
    Dependency(firrtl.transforms.RemoveReset),
    // try to work around dead code elimination removing our registers
    Dependency[firrtl.transforms.DeadCodeElimination]
  )

  override def invalidates(a: Transform) = false

  // since we generate PresetRegAnnotations, we need to run after preset propagation
  override def optionalPrerequisites = Seq(Dependency[PropagatePresetAnnotations])

  // we want to run before the actual Verilog is emitted
  override def optionalPrerequisiteOf = firrtl.stage.Forms.BackendEmitters

  sealed trait NodeType
  case class PrimaryInput(name: String) extends NodeType
  case class PrimaryOutput(name: String) extends NodeType
  case class Node(name: String) extends NodeType
  case class PrimOp(op: firrtl.ir.PrimOp) extends NodeType
  case class DefReg(name: String) extends NodeType

  override def execute(state: CircuitState): CircuitState = {
    val graph = new DefaultDirectedGraph[NodeType, DefaultEdge](classOf[DefaultEdge])
    val circuit = state.circuit
    val nameToVertex = mutable.Map[String, NodeType]()

    circuit.foreachModule { m =>
      m.foreachPort { p =>
        if (p.name == "clock" || p.name == "reset") {

        } else {
          p.direction match {
            case Input =>
              val vertex = PrimaryInput(p.name)
              graph.addVertex(vertex)
              nameToVertex.addOne(p.name -> vertex)
            case Output =>
              val vertex = PrimaryOutput(p.name)
              graph.addVertex(vertex)
              nameToVertex.addOne(p.name -> vertex)
          }
        }
      }
      m.foreachStmt { stmt =>
        traverseStatement(stmt, graph, nameToVertex)
      }
    }
    state.copy(annotations = state.annotations :+ GraphAnnotation(graph))
  }

  def traverseStatement(stmt: Statement, graph: DefaultDirectedGraph[NodeType, DefaultEdge], nameToVertex: mutable.Map[String, NodeType]): Unit = {
    stmt.foreachStmt {
      case Connect(info, loc, expr) =>
        val vertex1 = nameToVertex(expr.asInstanceOf[Reference].name)
        val vertex2 = nameToVertex(loc.asInstanceOf[Reference].name)
        graph.addEdge(vertex1, vertex2)
      case DefNode(info, name, expr) =>
        val thisNode = Node(name)
        graph.addVertex(thisNode)
        nameToVertex.addOne(name -> thisNode)
        val createdVertex = traverseExpr(expr, graph, nameToVertex)
        graph.addEdge(createdVertex, thisNode)
      case DefRegister(info, name, tpe, clock, reset, init) =>
        val thisNode = DefReg(name)
        graph.addVertex(thisNode)
        nameToVertex.addOne(name -> thisNode)
      case block@Block(stmts) =>
        traverseStatement(block, graph, nameToVertex)
    }

    stmt.foreachExpr { expr =>
      traverseExpr(expr, graph, nameToVertex)
    }
  }

  def traverseExpr(expression: Expression, graph: DefaultDirectedGraph[NodeType, DefaultEdge], nameToVertex: mutable.Map[String, NodeType]): NodeType = {
    expression match {
      case DoPrim(op, args: Seq[Expression], consts, tpe) =>
        op match {
          case Add | Sub | Mul =>
            val sourceVertices: Seq[NodeType] = Seq(args(0), args(1)).map {
              case Reference(name, tpe, kind, flow) =>
                nameToVertex(name)
            }
            val opVertex: NodeType = PrimOp(op)
            graph.addVertex(opVertex)
            graph.addEdge(sourceVertices(0), opVertex)
            graph.addEdge(sourceVertices(1), opVertex)
            opVertex
        }
    }
  }
}