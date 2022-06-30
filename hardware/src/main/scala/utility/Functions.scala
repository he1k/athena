package utility
import chisel3._
import chisel3.util._
object Functions {
  def createClock(f_in: Long, f_out: Int, clr: Bool = false.B): Bool ={
    val CNT_PERIOD = (f_in/f_out) // Calculate count's pr. period
    val CNT_MAX = (CNT_PERIOD - 1)/2
    val cnt = RegInit(0.U(log2Up(CNT_MAX).W))
    val clk = RegInit(true.B)
    cnt := cnt + 1.U
    when(cnt === CNT_MAX.U){
      cnt := 0.U
      clk := ~clk
    }
    when(clr){
      clk := true.B
      cnt := 0.U
    }
    clk
  }
  def getBits(x: Int, end: Int, start: Int): Int ={
    ((x << (32-(end+1))) >>> (32-(end+1))) >>> start
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
