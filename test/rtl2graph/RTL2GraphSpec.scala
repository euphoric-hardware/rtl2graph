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
}
