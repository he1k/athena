import chisel3._
import utility.Constants._
import chisel3.util._
class PWM(channels: Int) extends Module{
  val io = IO(new Bundle{
    val ch = Vec(channels, new Bundle{
      val set = Input(Bool())
      val max = Input(UInt(log2Up(PWM_MAX).W))
      val pwm = Output(Bool())
    })
  })
  val ch = RegInit(0.U.asTypeOf(Vec(channels, new Bundle {
    val cnt = UInt(log2Up(PWM_MAX).W)
    val max = UInt(log2Up(PWM_MAX).W)
  })))
  for(i <- 0 until channels){
    ch(i).cnt := Mux(ch(i).cnt === ch(i).max, 0.U, ch(i).cnt + 1.U)
    when(io.ch(i).set){
      ch(i).max := io.ch(i).max
    }
    io.ch(i).pwm := ch(i).cnt < ch(i).max
  }

}
