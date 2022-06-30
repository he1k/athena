package arithmetic
import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec

import java.lang.Float.{floatToIntBits, intBitsToFloat}
import utility.Functions.printFields
class FPAddRound extends Module{
  val io = IO(new Bundle{
    val a, b = Input(SInt(32.W))
    val y = Output(UInt(32.W))
    val sub = Input(Bool())
    val en = Input(Bool())
  })
  val fpAdd = Module(new FPAdd)
  fpAdd.io.en := io.en
  fpAdd.io.sub := io.sub
  val rnd = Module(new FPRound)
  rnd.io.a := fpAdd.io.y
  rnd.io.en := io.en
  rnd.io.rm := 0.U
  fpAdd.io.a := io.a.asUInt
  fpAdd.io.b := io.b.asUInt
  io.y := rnd.io.y
}

class FPAddSpec extends AnyFlatSpec with ChiselScalatestTester {
  "FPAdderSpec " should "pass" in {
    test(new FPAddRound).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      def input(a: Float, b: Float, sub: Boolean):Unit={
        dut.io.a.poke(floatToIntBits(a).S)
        dut.io.b.poke(floatToIntBits(b).S)
        dut.io.sub.poke(sub.B)
      }
      def printDetails(a: Float, b: Float, sum: Float, sub: Boolean): Unit ={
        print("Calculation: ")
        print(f"$a%1.30f ")
        if(sub){print("- ")}else{print("+ ")}
        print(f"$b%1.30f = ")
        println(f"$sum%1.30f")
        print(f"Fields of$a%1.30f:               ")
        printFields(floatToIntBits(a))
        print(f"Fields of$b%1.30f:               ")
        printFields(floatToIntBits(b))
        print("Fields of expected output:        ")
        printFields(floatToIntBits(sum))
        print("Fields of actual output:          ")
        printFields(floatToIntBits(intBitsToFloat(dut.io.y.peek().litValue.toInt)))
      }
      def step(steps: Int = 1):Unit={
        dut.clock.step(steps)
      }
      dut.io.en.poke(true.B)
      val r = new scala.util.Random(-1859080406)//-1623095995, -1859080406
    var misses = 0
      var sub = false
      val rounds = 30
      for(i<-1 to rounds)  {
        println("------------------------------------------ " + "Test: " + i.toString + " ------------------------------------------")
        val a = r.nextFloat()*1
        val b = r.nextFloat()*1000000000
        sub = !sub
        val expected = if (sub) {a - b} else {a + b}
        input(a, b, sub)
        step(20)
        val res = intBitsToFloat(dut.io.y.peek().litValue.toInt)
        val error = (res - expected) != 0
        if (error) {
          println("Incorrect result. Printing diagnostics:")
          printDetails(a,b,expected,sub)
          misses += 1
        } else{
          println("Correct result.")
        }

      }
      println("------------> MISS RATIO: " + misses.toString + " / " + rounds.toString + " = " + (misses.toFloat/rounds).toString)
    }
  }
}
