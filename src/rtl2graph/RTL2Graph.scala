package rtl2graph

// Compiler Infrastructure
import firrtl.PrimOps._
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.transforms.PropagatePresetAnnotations
import firrtl.{CircuitState, DependencyAPIMigration, Transform, Utils}
// Firrtl IR classes
import firrtl.ir.{DefModule, Expression, Mux, Statement}
// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable

object ToGraphPass extends Transform with DependencyAPIMigration {
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

  sealed trait Primitive

  case class PI(name: String) extends Primitive

  case class PO(name: String) extends Primitive

  case class NODE(name: String) extends Primitive

  object ADD extends Primitive

  case class Node(tpe: Primitive)

  case class Graph(nodes: mutable.Map[Int, Node], edges: mutable.Map[Int, Int])

  def nodeIdFromName(graph: Graph, name: String): Int = {
    val node: Option[(Int, Node)] = graph.nodes.find { case (id, node) =>
      node.tpe match {
        case PI(n) => n == name
        case PO(n) => n == name
        case NODE(n) => n == name
        case ADD => false
      }
    }
    node.get._1
  }

  class NodeIdSource {
    var id = 0

    def newId(): Int = {
      val i = id
      id += 1
      i
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    println(state.circuit.serialize)
    val graph = Graph(mutable.Map.empty, mutable.Map.empty)
    val nodeIdSource = new NodeIdSource
    val circuit = state.circuit

    circuit.foreachModule { m =>
      m.foreachPort { p =>
        if (p.name == "clock" || p.name == "reset") {

        } else {
          p.direction match {
            case Input =>
              graph.nodes.addOne(nodeIdSource.newId() -> Node(PI(p.name)))
            case Output =>
              graph.nodes.addOne(nodeIdSource.newId() -> Node(PO(p.name)))
          }
        }
      }
      m.foreachStmt { stmt =>
        traverseStatement(stmt, graph, nodeIdSource)
      }
    }
    println(graph)
    state
  }

  def traverseStatement(stmt: Statement, graph: Graph, nodeIdSource: NodeIdSource): Unit = {
    println(stmt)
    stmt.foreachStmt {
      case Connect(info, loc, expr) =>
        val node1 = nodeIdFromName(graph, expr.asInstanceOf[Reference].name)
        val node2 = nodeIdFromName(graph, loc.asInstanceOf[Reference].name)
        graph.edges.addOne(node1 -> node2)
        println(node1, node2)
        return
      case DefNode(info, name, expr) =>
        val createdNodeId = traverseExpr(expr, graph, nodeIdSource)
        val thisNode = Node(NODE(name))
        val thisNodeId = nodeIdSource.newId()
        graph.nodes.addOne(thisNodeId -> thisNode)
        graph.edges.addOne(createdNodeId.get -> thisNodeId)
      //println(info, name, value)
      case block@Block(stmts) =>
        traverseStatement(block, graph, nodeIdSource)
    }

    stmt.foreachExpr { expr =>
      traverseExpr(expr, graph, nodeIdSource)
    }
  }

  def traverseExpr(expression: Expression, graph: Graph, nodeIdSource: NodeIdSource): Option[Int] = {
    expression match {
      case DoPrim(op, args: Seq[Expression], consts, tpe) =>
        op match {
          case Add =>
            val nodeArgs = Seq(args(0), args(1)).map {
              case Reference(name, tpe, kind, flow) =>
                val nodeId = nodeIdFromName(graph, name)
                nodeId
            }
            val node = Node(ADD)
            val thisNodeId = nodeIdSource.newId()
            graph.nodes.addOne(thisNodeId -> node)
            graph.edges.addAll(Seq(
              nodeArgs(0) -> thisNodeId,
              nodeArgs(1) -> thisNodeId
            ))
            Some(thisNodeId)
        }
    }
  }
}