import chisel3._
import utility.Functions.create_clock
import utility.Constants._
import chisel3.util._

class I2C extends Module{
  val io = IO(new Bundle{
    val SCL = Output(Bool())
    val SDA = new Bundle{
      val I = Input(Bool())
      val O = Output(Bool())
    }
    val AD0 = Output(Bool())
    val cmd = new Bundle{
      val valid = Input(Bool())
      val ra = Input(UInt(8.W))
      val busy = Output(Bool())
      val ready = Output(Bool())
    }
  })
  // Create I2C clock:
  val CNT_PERIOD = (MAIN_FREQ/SCL_FREQ) // Calculate count's pr. period
  val HALF_PERIOD = (CNT_PERIOD - 1)/2
  val QUART_PERIOD = HALF_PERIOD/2
  val cnt = RegInit(0.U(log2Up(HALF_PERIOD).W))
  val scl = RegInit(true.B)
  val clr = WireDefault(true.B) // Just keep SCL high as default
  cnt := cnt + 1.U
  when(cnt === HALF_PERIOD.U){
    cnt := 0.U
    scl := ~scl
  }
  when(clr){
    cnt := 0.U
    scl := true.B
  }
  val chgData = (cnt === QUART_PERIOD.U) & (!scl)
  val cntBits = RegInit(0.U)
  val dout = RegInit(0.U)


  val slaveAddr = "b1101000".U // LSB needs to match pin AD0 voltage.
  val idle :: start :: sla :: ra :: Nil = Enum(4)
  val state = RegInit(idle)
  io.AD0 := false.B
  io.SDA.O := true.B
  io.SCL := scl
  switch(state){
    is(idle){
      when(io.cmd.valid){
        state := start
      }
    }
    is(start){ // START condition is a high to low transition on SDA when SCL remains high
      io.SDA.O := false.B
      clr := false.B // After a
      dout := slaveAddr
      when(!scl){
        state := sla
      }
    }
    is(sla){ // Send slave address and write signal. Wait for ack

    }
  }
}
