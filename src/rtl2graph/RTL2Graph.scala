package rtl2graph

// Compiler Infrastructure

import firrtl.PrimOps._
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Dependency, Unserializable}
import firrtl.transforms.PropagatePresetAnnotations
import firrtl.{CircuitState, DependencyAPIMigration, Transform, Utils}
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf
// Firrtl IR classes
import firrtl.ir.{DefModule, Expression, Mux, Statement}
// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable


object ToGraphPass extends Transform with DependencyAPIMigration {
  case class GraphAnnotation(graph: DefaultDirectedGraph[NodeType, EdgeType]) extends NoTargetAnnotation with Unserializable

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

  case class PrimOp(id: String, op: firrtl.ir.PrimOp) extends NodeType {
    override def toString: String = { op.toString }
  }

  case class DefReg(name: String) extends NodeType
  case class DefMemory(name:String) extends NodeType

  case class Mux(id: String, info: Info, tpe: firrtl.ir.Type) extends NodeType {
    override def toString: String = { "Mux" }
  }

  case class UIntLiteral(value: BigInt, width: Width) extends NodeType

  class EdgeType extends DefaultEdge {
    override def toString: String = {
      this.getDOTLabel
    }

    def getDOTLabel: String = {
      this.getClass.getSimpleName
    }
  }

  class LeftArgument() extends EdgeType

  class RightArgument() extends EdgeType

  case class LeftParam() extends EdgeType

  case class RightParam() extends EdgeType

  case class SelectEdge() extends EdgeType

  override def execute(state: CircuitState): CircuitState = {
    val graph = new DefaultDirectedGraph[NodeType, EdgeType](classOf[EdgeType])
    val circuit = state.circuit
    // TODO: Are names unique across modules? If not, this will break.
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
      firstModulePass(m, graph, nameToVertex)
      m.foreachStmt { stmt =>
        traverseStatement(stmt, graph, nameToVertex)
      }
    }
    state.copy(annotations = state.annotations :+ GraphAnnotation(graph))
  }

  def firstModulePass(m: DefModule, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[String, NodeType]): Unit = {
    m.foreachStmt {
      createStatementNodes(_, graph, nameToVertex)
    }
  }

  def createStatementNodes(stmt: Statement, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[String, NodeType]): Unit = {
    stmt.foreachStmt {
      case declaration: IsDeclaration =>
        val name = declaration.name
        var node: NodeType = Node(name)
        declaration match {
          case _: firrtl.ir.DefRegister =>
            node = DefReg(name)
          case _: firrtl.ir.DefMemory =>
            node = DefMemory(name)
          case default =>
            node = Node(name)
        }
        graph.addVertex(node)
        nameToVertex.addOne(name -> node)
      case block: Block =>
        createStatementNodes(block, graph, nameToVertex)
      case _ =>
    }
  }

  // TODO: eliminate global variable
  var curNodeName = "this should never be seen in the output.";
  def traverseStatement(stmt: Statement, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[String, NodeType]): Unit = {
    stmt.foreachStmt {
      case Connect(info, loc, expr) =>
        val vertex1 = nameToVertex(expr.asInstanceOf[Reference].name)
        val vertex2 = nameToVertex(loc.asInstanceOf[Reference].name)
        graph.addEdge(vertex1, vertex2)
      case DefNode(info, name, expr) =>
        val thisNode = nameToVertex(name)
        curNodeName = name
        val createdVertex = traverseExpr(expr, graph, nameToVertex, info)
        graph.addEdge(createdVertex, thisNode)
      case block@Block(stmts) =>
        traverseStatement(block, graph, nameToVertex)
      case _: IsDeclaration => {}
      case EmptyStmt => {}
    }

    stmt.foreachExpr { expr =>
      var flag: Boolean = false
      // There should just be one info, so we do something funny with a flag.
      stmt.foreachInfo { info =>
        assert(!flag)
        traverseExpr(expr, graph, nameToVertex, info)
        flag = true
      }
    }
  }

  def traverseExpr(
                    expression: Expression,
                    graph: DefaultDirectedGraph[NodeType, EdgeType],
                    nameToVertex: mutable.Map[String, NodeType],
                    stmtInfo: Info,
                  ): NodeType = {
    expression match {
      case DoPrim(op, args: Seq[Expression], consts, tpe) =>
        op match {
          case Add | Sub | Mul | Or | Eq | And =>
            val sourceVertices: Seq[NodeType] = Seq(args(0), args(1)).map {
              traverseExpr(_, graph, nameToVertex, stmtInfo)
            }
            val opVertex: NodeType = PrimOp(curNodeName, op)
            graph.addVertex(opVertex)
            graph.addEdge(sourceVertices(0), opVertex, new LeftArgument())
            graph.addEdge(sourceVertices(1), opVertex, new RightArgument())
            opVertex
          case Tail =>
            val sourceVertex: NodeType = traverseExpr(args.head, graph, nameToVertex, stmtInfo)
            val opVertex: NodeType = PrimOp(curNodeName, op)
            graph.addVertex(opVertex)
            graph.addEdge(sourceVertex, opVertex, new LeftArgument())
            opVertex
        }
      case Reference(name, tpe, kind, flow) =>
        nameToVertex(name)
      case firrtl.ir.Mux(cond, tval, fval, tpe) =>
        val conditionVertex = traverseExpr(cond, graph, nameToVertex, stmtInfo)
        val tvalVertex = traverseExpr(tval, graph, nameToVertex, stmtInfo)
        val fvalVertex = traverseExpr(fval, graph, nameToVertex, stmtInfo)
        val muxVertex: NodeType = Mux(curNodeName, stmtInfo, tpe)
        graph.addVertex(muxVertex)
        graph.addEdge(conditionVertex, muxVertex)
        graph.addEdge(tvalVertex, muxVertex)
        graph.addEdge(fvalVertex, muxVertex)
        muxVertex
      case firrtl.ir.UIntLiteral(value, width) =>
        val literalVertex = UIntLiteral(value, width)
        graph.addVertex(literalVertex)
        literalVertex
    }
  }
}