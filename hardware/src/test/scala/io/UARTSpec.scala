package io

import chiseltest._
import utility.Constants._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
class UARTSpec extends AnyFlatSpec with ChiselScalatestTester {
  val size = 16
  val steps = CPB.toInt
  "UART" should "pass" in {
    test(new UART(size)).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      def rxWrite(data: Int): Unit ={
        var dat = data
        dut.io.rx.poke(false.B) // start bit
        dut.clock.step(steps)
        for(i <- 0 until 8){
          dut.io.rx.poke(((dat & 0x00000001) == 1).B)
          dut.clock.step(steps)
          dat = dat >>> 1
        }
        dut.io.rx.poke(true.B) // stop bit
        dut.clock.step(steps)
      }
      def txRead(data: Int): Unit ={
        var dat = 0
        while(dut.io.tx.peekBoolean()){ // Wait for start bit
          dut.clock.step()
        }
        dut.clock.step((3*steps)/2) // Start bit
        for(i <- 0 until 8){
          dat = dat | ((if(dut.io.tx.peekBoolean()){1}else{0}) << i)
          dut.clock.step(steps)
        }
        dut.clock.step((steps)/2) // Stop bit
        println("Expect :" + data + ", got :" + dat)
        assert(dat == data)
      }
      for(i <- 0 to size){ // Write 1 more byte than the fifo will fit to check behaviour
        rxWrite(i)
      }
      dut.clock.step(10)
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.we.poke(false.B)
      while(!dut.io.cmd.ready.peekBoolean()){dut.clock.step()}
      dut.io.cmd.dout.expect(0x00.U)
      dut.io.cmd.we.poke(true.B)

      for(i <- 0 until size){ // Fill up tx buffer
        dut.io.cmd.din.poke(i.U)
        while(!dut.io.cmd.ready.peekBoolean()){dut.clock.step()}
        dut.clock.step()
      }
      for(i <- 0 until size){ // Read tx buffer
        txRead(i)
      }

    }
  }
}
