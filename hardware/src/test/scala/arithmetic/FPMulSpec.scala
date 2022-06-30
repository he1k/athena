package arithmetic
import utility.Functions.printFields
import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalactic.TimesOnInt.convertIntToRepeater

import java.lang.Float.floatToIntBits
import java.lang.Float.intBitsToFloat
import scala.util.control.Breaks._

class FPMulRound extends Module{
  val io = IO(new Bundle{
    val a, b = Input(SInt(32.W))
    val y = Output(UInt(32.W))
    val en = Input(Bool())
  })
  val fpMul = Module(new FPMul)
  fpMul.io.en := io.en
  val rnd = Module(new FPRound)
  rnd.io.a := fpMul.io.y
  rnd.io.en := io.en
  rnd.io.rm := 0.U
  fpMul.io.a := io.a.asUInt
  fpMul.io.b := io.b.asUInt
  io.y := rnd.io.y
}

class FPMulSpec extends AnyFlatSpec with ChiselScalatestTester {
  "FPMul" should "pass" in {
    test(new FPMulRound).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      def input(a: Float, b: Float):Unit={
        dut.io.a.poke(floatToIntBits(a).S)
        dut.io.b.poke(floatToIntBits(b).S)
      }
      def output():Unit={
        val res = intBitsToFloat(dut.io.y.peek().litValue.toInt)
        println(f"Output = $res%1.30f")
      }
      def step(x: Int = 1):Unit={
        dut.clock.step(x)
      }
      dut.io.en.poke(true.B)
      val r = new scala.util.Random() // 1420
      val rounds = 100
      var misses = 0
      breakable {
        rounds times {
          val a = r.nextFloat()*1000
          val b = r.nextFloat()
          val expct = a * b
          input(a, b)
          step(7)
          val res = intBitsToFloat(dut.io.y.peek().litValue.toInt)
          val error = (res - expct) != 0
          if (error) {
            println("a : " + a + ", b : " + b)
            misses += 1
            print("Fields expected:")
            printFields(floatToIntBits(a * b))
            print("Fields result:  ")
            printFields(floatToIntBits(res))
          }
          println("--------------------------------------------------------------------")
        }
      }
      println("MISS RATIO: " + misses.toString + " / " + rounds.toString + " ≈ " (misses/rounds).toString)
    }
  }
}