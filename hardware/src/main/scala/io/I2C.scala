package io

import chisel3._
import chisel3.util._
import utility.Constants._


class I2CDriver extends Module{
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
      val ra = Input(UInt(8.W))
      val sla = Input(UInt(7.W))
      val din = Input(UInt(8.W))
      val busy = Output(Bool())
      val ready = Output(Bool())
      val dout = Output(UInt(16.W))
    }
  })
  // Create I2C clock:
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
  val midLow = (cnt === QUART_PERIOD.U) & (!scl)
  val midHigh = (cnt === QUART_PERIOD.U) & (scl)
  val posedge = RegNext((!scl) & (cnt === HALF_PERIOD.U))
  val cntBits = RegInit(0.U(4.W))
  val cntTx = RegInit(0.U(2.W))
  val sda = RegInit(0.U(8.W))
  val din = RegInit(0.U(16.W))


  val slaveAddr = "b1101000".U // LSB needs to match pin AD0 voltage.
  val idle :: initS :: p :: readS :: tx :: rx :: ackRx :: ackTx :: Nil = Enum(8)
  val state = RegInit(idle)
  val nextState = RegInit(idle)
  val ack = RegInit(false.B)
  io.AD0 := io.cmd.sla(0)
  io.SDA.O := true.B
  io.SDA.out := true.B
  io.SCL := scl
  io.cmd.busy := false.B
  io.cmd.ready := false.B
  io.cmd.dout := din
  // Debugging
  println("States: ")
  println("idle: " + idle.litValue.toInt.toBinaryString)
  println("initS:" + initS.litValue.toInt.toBinaryString)
  println("p:    " + p.litValue.toInt.toBinaryString)
  println("readS:" + readS.litValue.toInt.toBinaryString)
  println("tx:   " + tx.litValue.toInt.toBinaryString)
  println("rx:   " + rx.litValue.toInt.toBinaryString)
  println("ackRx:" + ackRx.litValue.toInt.toBinaryString)
  println("ackTx:" + ackTx.litValue.toInt.toBinaryString)

  switch(state) {
    is(idle) { // Wait for valid command
      clrSCL := true.B // Keep SCL high
      io.SDA.O := true.B
      io.cmd.ready := true.B
      when(io.cmd.valid) { // Latch slave address and send start condition
        state := initS
        sda := io.cmd.sla ## 0.U // Slave address is 7 bits wide. Add write bit
      }
    }
    is(initS) { // Do a high to low transition on SDA while SCL is high
      // Wait until SCL completes one quarter of a cycle
      when(cnt >= QUART_PERIOD.U | (!scl)) {
        io.SDA.O := false.B
      }
      when((cnt === (QUART_PERIOD).U) & !scl) {
        state := tx
      }
    }
    is(readS){ // Do a high to low transition on SDA while SCL is high
               // Clock is midway through low period. Wait for it to enter high period,
               // then same as initS
      io.SDA.O := true.B
      when(scl){
        cntBits := 1.U // Reusing bit counter for extended clock awareness
      }
      when(cntBits === 1.U){
        when(cnt >= QUART_PERIOD.U | (!scl)) {
          io.SDA.O := false.B
        }
        when((cnt === (QUART_PERIOD).U) & !scl) {
          state := tx
          cntBits := 0.U
        }
      }
    }
    is(p) {
      io.SDA.O := false.B
      when((cnt === QUART_PERIOD.U) & !scl){
        cntBits := 0.U
      }
      when((cnt === QUART_PERIOD.U) & scl){ // Now SCL must remain high
        cntTx := 0.U
      }
      when(cntTx === 0.U){
        io.SDA.O := true.B
        when(cnt === HALF_PERIOD.U){
          clrSCL := true.B
          state := idle
        }
      }
      when(cntBits === 0.U){
        io.SDA.out := true.B
      }
    }
    is(tx) {
      io.SDA.O := sda(7)
      when((cnt === QUART_PERIOD.U) & !scl) {
        cntBits := cntBits + 1.U
        sda := sda(6, 0) ## 0.U
        when(cntBits === 7.U){
          cntBits := 0.U
          state := ackRx
          switch(cntTx){
            is(0.U) { // ad+w has been sent
              sda := io.cmd.ra
              nextState := tx
            }
            is(1.U) { // ra has been sent
              when(!io.cmd.we) {
                nextState := readS
                sda := io.cmd.ra
              }.otherwise {
                sda := io.cmd.din
              }
            }
            is(2.U) {
              when(!io.cmd.we) {
                nextState := rx
              }.otherwise {
                nextState := p
              }
            }
          }
        }
      }
    }
    is(rx) {
      io.SDA.out := false.B
      when(midHigh) { // Maybe sample at rising edge instead?
        cntBits := cntBits + 1.U
        when(cntBits =/= 8.U){
          din := din(14, 0) ## io.SDA.I
        }
      }
      when(cntBits === 8.U & midLow){
        cntBits := 0.U
        state := ackTx
        ack := cntTx =/= 3.U
        nextState := Mux(cntTx =/= 3.U, p, rx)
      }
    }
    is(ackRx){
      io.SDA.out := false.B
      when(midHigh){
        ack := !io.SDA.I
      }
      when(midLow){
        state := Mux(ack, nextState, idle)
        cntTx := cntTx + 1.U
      }
    }
    is(ackTx){
      io.SDA.out := true.B
      io.SDA.O := ack
      when(midLow){
        state := nextState
        cntTx := cntTx + 1.U
      }
    }
  }
}
