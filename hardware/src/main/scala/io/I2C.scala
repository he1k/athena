package io

import chisel3._
import chisel3.util._
import utility.Constants._

class I2C extends Module{
  val io = IO(new Bundle{
    val SCL = Output(Bool())
    val SDA = new Bundle{
      val I = Input(Bool())
      val O = Output(Bool())
      val out = Output(Bool())
    }
    val AD0 = Output(Bool())
    val cmd = new Bundle{
      val valid = Input(Bool())
      val we = Input(Bool())
      val din = Input(UInt(8.W))
      val busy = Output(Bool())
      val ready = Output(Bool())
      val dout = Output(UInt(8.W))
    }
  })
  // Create I2C clock:
  val CNT_PERIOD = (MAIN_FREQ/SCL_FREQ) // Calculate count's pr. period
  val HALF_PERIOD = (CNT_PERIOD - 1)/2
  val QUART_PERIOD = HALF_PERIOD/2
  val cnt = RegInit(0.U(log2Up(HALF_PERIOD).W))
  val scl = RegInit(true.B)
  val clr = WireDefault(false.B) // Just keep SCL high as default
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
  val posedge = RegNext((!scl) & (cnt === HALF_PERIOD.U))
  val cntBits = RegInit(0.U(4.W))
  val dout = RegInit(0.U(8.W))


  val slaveAddr = "b1101000".U // LSB needs to match pin AD0 voltage.
  val idle :: start :: sla :: ra :: ack :: tx :: Nil = Enum(6)
  val state = RegInit(idle)
  val nextState = RegInit(idle)
  io.AD0 := 0.U // TODO add dynamic address LSB
  io.SDA.O := true.B
  io.SDA.out := true.B
  io.SCL := scl
  io.cmd.busy := false.B
  io.cmd.ready := false.B
  switch(state){
    is(idle){
      clr := true.B
      when(io.cmd.valid){
        state := start
      }
    }
    is(start){ // START condition is a high to low transition on SDA when SCL remains high
      io.SDA.O := false.B
      when(chgData){
        dout := io.cmd.din(6,0) ## 0.U
        state := sla
      }
      io.SDA.O := dout(7)
    }
    is(sla){ // Send slave address and write signal. Wait for ack
      when(chgData){
        dout := dout(6, 0) ## 0.U
        cntBits := cntBits + 1.U
        when(cntBits === 7.U){ // Done sending address and write bit. Now we wait for acknowledge from slave
          state := ack
          cntBits := 0.U
        }
      }
      io.SDA.O := dout(7)
    }
    is(ack){
      io.SDA.out := false.B
      when(posedge | io.SCL){
        clr := true.B
        when(io.SDA.I){
          state := ra
          dout := io.cmd.din
        } .otherwise{
          state := idle
        }
      }
      when(io.SCL){
        when(io.SDA.I){
          io.cmd.ready := true.B
          when(io.cmd.valid){
            dout := io.cmd.din
            state := ra
          } .otherwise{
            state := idle
            io.cmd.busy := true.B
          }
        }
      }
    }
    is(ra){
      io.SDA.O := dout(7)
      when(chgData){
        cntBits := cntBits + 1.U
        when(cntBits =/= 0.U){
          dout := dout(6,0) ## 0.U
        }
        when(cntBits === 7.U){

        }
      }
    }
  }
}
class I2CMaster extends Module{
  val io = IO(new Bundle{
    val SCL = Output(Bool())
    val SDA = new Bundle{
      val I = Input(Bool())
      val O = Output(Bool())
      val out = Output(Bool())
    }
    val AD0 = Output(Bool())
    val cmd = new Bundle{
      val valid = Input(Bool())
      val we = Input(Bool())
      val ra = Input(UInt(7.W))
      val sla = Input(UInt(7.W))
      val din = Input(UInt(8.W))
      val busy = Output(Bool())
      val ready = Output(Bool())
      val dout = Output(UInt(8.W))
    }
  })
  // Create I2C clock:
  val CNT_PERIOD = (MAIN_FREQ/SCL_FREQ) // Calculate count's pr. period
  val HALF_PERIOD = (CNT_PERIOD - 1)/2
  val QUART_PERIOD = HALF_PERIOD/2
  val cnt = RegInit(0.U(log2Up(HALF_PERIOD).W))
  val scl = RegInit(true.B)
  val clrSCL = WireDefault(false.B) // Just keep SCL high as default
  cnt := cnt + 1.U
  when(cnt === HALF_PERIOD.U){
    cnt := 0.U
    scl := ~scl
  }
  when(clrSCL){
    cnt := 0.U
    scl := true.B
  }
  val chgData = (cnt === QUART_PERIOD.U) & (!scl)
  val posedge = RegNext((!scl) & (cnt === HALF_PERIOD.U))
  val cntBits = RegInit(0.U(4.W))
  val sda = RegInit(0.U(8.W))


  val slaveAddr = "b1101000".U // LSB needs to match pin AD0 voltage.
  val idle :: s :: p :: rxAck :: txAck :: tx :: rx :: Nil = Enum(6)
  val state = RegInit(idle)
  val nextState = RegInit(idle)
  val write = RegInit(false.B)
  io.AD0 := 0.U // TODO add dynamic address LSB
  io.SDA.O := sda(7)
  io.SDA.out := true.B
  io.SCL := scl
  io.cmd.busy := false.B
  io.cmd.ready := false.B
  // For both writes and reads, the state machine executes the sequence: s, ad+w, ack, ra, ack.
  // For writes, it further does: data, ack, p
  // For reads, it further does: s, ad+r

  switch(state) {
    is(idle) { // Wait for valid command
      clrSCL := true.B // Keep SCL high
      io.SDA.O := true.B
      io.cmd.ready := true.B
      when(io.cmd.valid){ // Latch slave address and send start condition
        state := s
        sda := io.cmd.sla ## 0.U // Slave address is 7 bits wide. Add write bit
        write := io.cmd.we
      }
    }
  }
  is(s){ // Do a high to low transition on SDA while SCL is high
    // Wait until SCL completes one quarter of a cycle
    when(cnt >= QUART_PERIOD.U){
      io.SDA.O := false.B
    }
    when(cnt === (HALF_PERIOD + QUART_PERIOD - 2).U){
      state := tx
    }
  }
  is(p){

  }
  is(tx){
    io.SDA.O := sda(7)
    when(cnt === (HALF_PERIOD + QUART_PERIOD).U){ // Clock is mid way through low time
      cntBits := cntBits + 1.U
      when(cntBits =/= 0.U){
        sda := sda(6,0) ## 0.U
      }
      when(cntBits === 7.U){
        state := rxAck
      }
    }
  }
  is(rx){

  }
  is(rxAck){
    when(cnt === HALF_PERIOD.U){ // Clock is mid way through high time
      when(!io.SDA.I){
        state := nextState
      }
    }
  }
  is(txAck){

  }
}
