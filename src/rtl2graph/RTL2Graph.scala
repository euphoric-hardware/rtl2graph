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

// define Graph Extraction Pass
object ToGraphPass extends Transform with DependencyAPIMigration {
  case class GraphAnnotation(graph: DefaultDirectedGraph[NodeType, EdgeType]) extends NoTargetAnnotation with Unserializable

  // prereqs: run on lowered firrtl
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

  case class Identifier(module: String, name: String)

  // Define Node types
  sealed trait NodeType {
    val id: Identifier
    val info: Info

    override def toString: String = {
      id.toString + ";" + info.toString
    }
  }
  case class PrimaryInput(id: Identifier, info: Info) extends NodeType
  case class PrimaryOutput(id: Identifier, info: Info) extends NodeType
  case class Node(id: Identifier, info: Info) extends NodeType
  case class PrimOp(id: Identifier,  info: Info, op: firrtl.ir.PrimOp) extends NodeType {
    override def toString: String = { op.toString }
  }
  case class DefReg(id: Identifier,  info: Info) extends NodeType
  case class DefMemory(id: Identifier,  info: Info, writers: Seq[String], readers: Seq[String], readwriters: Seq[String]) extends NodeType
  case class Mux(id: Identifier, info: Info, tpe: firrtl.ir.Type) extends NodeType {
    override def toString: String = { "Mux" }
  }
  case class ValidIf(id: Identifier,  info: Info) extends NodeType;
  case class IsInvalid(id: Identifier,  info: Info) extends NodeType;
  case class UIntLiteral(id: Identifier,  info: Info, value: BigInt, width: Width) extends NodeType
  case class SIntLiteral(id: Identifier,  info: Info, value: BigInt, width: Width) extends NodeType

  // Define Edge types
  class EdgeType extends DefaultEdge {
    override def toString: String = {
      this.getDOTLabel
    }

    def getDOTLabel: String = {
      this.getClass.getSimpleName
    }
  }
  // class LeftArgument() extends EdgeType
  // class RightArgument() extends EdgeType
  // case class LeftParam() extends EdgeType
  // case class RightParam() extends EdgeType
  // case class SelectEdge() extends EdgeType
  class LeftArgument() extends EdgeType
  class RightArgument() extends EdgeType
  class ConditionEdge() extends EdgeType
  class TValEdge() extends EdgeType
  class FValEdge() extends EdgeType
  // 
  override def execute(state: CircuitState): CircuitState = {
    val graph = new DefaultDirectedGraph[NodeType, EdgeType](classOf[EdgeType])
    val circuit = state.circuit
    // TODO: Are names unique across modules? If not, this will break.
    val nameToVertex = mutable.Map[Identifier, NodeType]()
    var annotations = state.annotations

    // first pass = build module hierarchy
    // second pass = find which graphs belong to which in hierarchy
    circuit.foreachModule { m =>
      val subGraph = new DefaultDirectedGraph[NodeType, EdgeType](classOf[EdgeType])
      m.foreachPort { p =>
        if (false) {

        } else {
          val id = Identifier(m.name, p.name)
          println(p.info)
          p.direction match {
            case Input =>
              val vertex = PrimaryInput(id, p.info)
              graph.addVertex(vertex)
              subGraph.addVertex(vertex)
              nameToVertex.addOne(id -> vertex)
            case Output =>
              val vertex = PrimaryOutput(id, p.info)
              graph.addVertex(vertex)
              subGraph.addVertex(vertex)
              nameToVertex.addOne(id -> vertex)
          }
        }
      }
      firstModulePass(m, graph, nameToVertex)
      firstModulePass(m, subGraph, nameToVertex)
      m.foreachStmt { stmt =>
        traverseStatement(m, stmt, graph, nameToVertex)
        traverseStatement(m, stmt, subGraph, nameToVertex)
      }
      annotations = annotations :+ GraphAnnotation(subGraph)
    }
    state.copy(annotations = annotations :+ GraphAnnotation(graph))
  }

  def firstModulePass(m: DefModule, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[Identifier, NodeType]): Unit = {
    m.foreachStmt {
      createStatementNodes(m, _, graph, nameToVertex)
    }
  }

  def createStatementNodes(m: DefModule, stmt: Statement, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[Identifier, NodeType]): Unit = {
    stmt.foreachStmt {
      case declaration: IsDeclaration =>
        val id = Identifier(m.name, declaration.name)
        var node: NodeType = Node(id, declaration.info)
        declaration match {
          case _: firrtl.ir.DefRegister =>
            node = DefReg(id, declaration.info)
          case dMem: firrtl.ir.DefMemory =>
            node = DefMemory(id, declaration.info, writers = dMem.writers, readers = dMem.readers, readwriters = dMem.readwriters)
          case dMod: firrtl.ir.DefModule =>
            // TODO
          case default =>
            node = Node(id, declaration.info)
        }
        graph.addVertex(node)
        nameToVertex.addOne(id -> node)
      case block: Block =>
        createStatementNodes(m, block, graph, nameToVertex)
      case _ =>
    }
  }

  def referenceLikeToReference(ref: firrtl.ir.RefLikeExpression): Reference = {
    ref match {
      case ref: Reference =>
        ref
      case subField: SubField =>
        referenceLikeToReference(subField.expr.asInstanceOf[firrtl.ir.RefLikeExpression])
    }
  }

  // TODO: eliminate global variable
  var curNodeName = "this should never be seen in the output.";
  def traverseStatement(m: DefModule, stmt: Statement, graph: DefaultDirectedGraph[NodeType, EdgeType], nameToVertex: mutable.Map[Identifier, NodeType]): Unit = {
    stmt.foreachStmt {
      case Connect(info, loc, expr) =>
        val vertex1 = expr match {
          case refLike: RefLikeExpression =>
            nameToVertex(Identifier(m.name, referenceLikeToReference(refLike).name))
          case expression: Expression =>
            traverseExpr(m, expression, graph, nameToVertex, info)
        }
        val vertex2 = loc match {
          case refLikeExpression: RefLikeExpression =>
            nameToVertex(Identifier(m.name, referenceLikeToReference(refLikeExpression).name))
        }
        graph.addEdge(vertex1, vertex2)
      case firrtl.ir.IsInvalid(info, expr) =>
        val vertex1 = expr match {
          case refLike: RefLikeExpression =>
            nameToVertex(Identifier(m.name, referenceLikeToReference(refLike).name))
          case expression: Expression =>
            traverseExpr(m, expression, graph, nameToVertex, info)
        }
        val vertex2 = IsInvalid(Identifier(m.name, vertex1.id.name), info)
        graph.addVertex(vertex2)
        graph.addEdge(vertex1, vertex2)
      case DefNode(info, name, expr) =>
        val thisNode = nameToVertex(Identifier(m.name, name))
        curNodeName = name
        val createdVertex = traverseExpr(m, expr, graph, nameToVertex, info)
        graph.addEdge(createdVertex, thisNode)
      case block@Block(stmts) =>
        traverseStatement(m, block, graph, nameToVertex)
      case _: IsDeclaration => {}
      case EmptyStmt => {}
    }

    stmt.foreachExpr { expr =>
      var flag: Boolean = false
      // There should just be one info, so we do something funny with a flag.
      stmt.foreachInfo { info =>
        assert(!flag)
        traverseExpr(m, expr, graph, nameToVertex, info)
        flag = true
      }
    }
  }

  def traverseExpr( m: DefModule,
                    expression: Expression,
                    graph: DefaultDirectedGraph[NodeType, EdgeType],
                    nameToVertex: mutable.Map[Identifier, NodeType],
                    stmtInfo: Info,
                  ): NodeType = {
    expression match {
      case DoPrim(op, args: Seq[Expression], consts, tpe) =>
        op match {
          case Add | Sub | Div | Mul | Or | And | Xor | Eq | Neq | Cat | Leq | Dshr | Dshl | Lt | Gt | Geq | Eq | Neq | Rem =>
            val sourceVertices: Seq[NodeType] = Seq(args(0), args(1)).map {
              traverseExpr(m, _, graph, nameToVertex, stmtInfo)
            }
            val opVertex: NodeType = PrimOp(Identifier(m.name, curNodeName), stmtInfo, op)
            graph.addVertex(opVertex)
            graph.addEdge(sourceVertices(0), opVertex, new LeftArgument())
            graph.addEdge(sourceVertices(1), opVertex, new RightArgument())
            opVertex
          case Tail | Bits | Andr | Orr | Xorr | Not | AsUInt | Neg | Cvt | Shr | Shl | AsAsyncReset | AsClock | AsFixedPoint | AsSInt | Pad =>
            val sourceVertex: NodeType = traverseExpr(m, args.head, graph, nameToVertex, stmtInfo)
            val opVertex: NodeType = PrimOp(Identifier(m.name, curNodeName),stmtInfo, op)
            graph.addVertex(opVertex)
            graph.addEdge(sourceVertex, opVertex, new LeftArgument())
            opVertex
        }
      case firrtl.ir.ValidIf(cond, value, tpe) =>
        val validIfVertex: NodeType = ValidIf(Identifier(m.name, curNodeName), stmtInfo)
        graph.addVertex(validIfVertex)
        graph.addEdge(validIfVertex, traverseExpr(m, cond, graph, nameToVertex, stmtInfo))
        graph.addEdge(validIfVertex, traverseExpr(m, value, graph, nameToVertex, stmtInfo))
        validIfVertex
      case refLike: RefLikeExpression =>
        nameToVertex(Identifier(m.name, referenceLikeToReference(refLike).name))
      case firrtl.ir.Mux(cond, tval, fval, tpe) =>
        val conditionVertex = traverseExpr(m, cond, graph, nameToVertex, stmtInfo)
        val tvalVertex = traverseExpr(m, tval, graph, nameToVertex, stmtInfo)
        val fvalVertex = traverseExpr(m, fval, graph, nameToVertex, stmtInfo)
        val muxVertex: NodeType = Mux(Identifier(m.name, curNodeName), stmtInfo, tpe)
        graph.addVertex(muxVertex)
        graph.addEdge(conditionVertex, muxVertex)
        graph.addEdge(tvalVertex, muxVertex)
        graph.addEdge(fvalVertex, muxVertex)
        muxVertex
      case firrtl.ir.UIntLiteral(value, width) =>
        val literalVertex = UIntLiteral(Identifier(m.name, value.toString + width.toString), stmtInfo,  value, width)
        graph.addVertex(literalVertex)
        literalVertex
      case firrtl.ir.SIntLiteral(value, width) =>
        val literalVertex = SIntLiteral(Identifier(m.name, value.toString + width.toString), stmtInfo,  value, width)
        graph.addVertex(literalVertex)
        literalVertex
    }
  }
}
