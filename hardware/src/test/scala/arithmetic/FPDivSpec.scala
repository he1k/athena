package arithmetic
import arithmetic.FPDivSpec.printFields
import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalactic.TimesOnInt.convertIntToRepeater

import scala.math.{abs, pow}
import java.lang.Float.floatToIntBits
import java.lang.Float.intBitsToFloat
import scala.util.control.Breaks._

object FPDivSpec{

  def getBits(x: Int, end: Int, start: Int): Int ={
    ((x << (32-(end+1))) >>> (32-(end+1))) >>> start
  }
  def binToFP(x: Int): Float={
    val s = getBits(x, 31, 31)
    val e = getBits(x, 30, 23)
    val m = getBits(x, 22, 0)
    var mDec : Float = 0
    if(e == 0 && m == 0){
      0.toFloat
    } else{
      for(i <- 0 to 22){
        mDec = mDec + ((m >>> i) & 0x00000001)*pow(2, -(23-i)).toFloat
      }
      (pow(-1, s)*(1+mDec)*pow(2, (e-127))).toFloat
    }
  }
  def FPToBin(x: Float): Int={
    val bits = floatToIntBits(x)
    val s = getBits(bits, 31, 31)
    val e = getBits(bits, 30, 23)
    val m = getBits(bits, 22, 0)
    (s << 31) + (e << 23) + m
  }
  def printFields(x: Int): Unit={
    val s = getBits(x, 31, 31).toBinaryString
    val e = getBits(x, 30, 23).toBinaryString
    val m = getBits(x, 22, 0).toBinaryString
    println("s = " + s)
    println("e = " + e)
    println("m = " + m)
  }
}
class FPDivRound(width: Int = 5) extends Module{
  val io = IO(new Bundle{
    val a, b = Input(SInt(32.W))
    val y = Output(UInt(32.W))
    val en = Input(Bool())
  })
  val fpDiv = Module(new FPDiv(width)) //FPDiv(depth, width))
  fpDiv.io.en := io.en
  val rnd = Module(new FPRound)
  rnd.io.a := fpDiv.io.y
  rnd.io.en := io.en
  rnd.io.rm := 0.U
  fpDiv.io.a := io.a
  fpDiv.io.b := io.b
  io.y := rnd.io.y
}

class FPDivSpec extends AnyFlatSpec with ChiselScalatestTester {
  "FPDivider" should "pass" in {
    test(new FPDivRound(5)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
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
      val rounds = 20
      var misses = 0
      breakable {
        rounds times {
          val a = r.nextFloat()*1000
          val b = r.nextFloat()
          val expct = a / b
          input(a, b)
          step(5)
          dut.io.en.poke(false.B)
          step(5)
          dut.io.en.poke(true.B)
          step(12)
          val res = intBitsToFloat(dut.io.y.peek().litValue.toInt)
          val error = (res - expct) != 0
          if (error) {
            println("a : " + a + ", b : " + b)
            misses += 1
            print("Fields expected:")
            printFields(floatToIntBits(a / b))
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