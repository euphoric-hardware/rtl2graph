package rtl2graph

import org.scalatest.freespec.AnyFreeSpec
import chisel3._
import chisel3.util._
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation

class RTL2GraphSpec extends AnyFreeSpec with CompilerTest {
  class Adder extends Module {
    val io = IO(new Bundle {
      val a = Input(UInt(8.W))
      val b = Input(UInt(8.W))
      val c = Output(UInt(9.W))
    })
    io.c := io.a +& io.b
  }

  "verify rtl2graph works" in {
    val (str, annos) = compile(new Adder(), "verilog", List(RunFirrtlTransformAnnotation(ToGraphPass)))
  }

  "rtl2graph with firrtl string" in {
    val firrtlStr =
      """
        |circuit Adder :
        |  module Adder :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_a : UInt<8>
        |    input io_b : UInt<8>
        |    output io_c : UInt<9>
        |
        |    node _io_c_T = add(io_a, io_b) @[RTL2GraphSpec.scala 16:18]
        |    io_c <= _io_c_T @[RTL2GraphSpec.scala 16:10]
        |""".stripMargin
    val (str, annos) = fromFirrtlString(firrtlStr, List(RunFirrtlTransformAnnotation(ToGraphPass)))
  }
}
