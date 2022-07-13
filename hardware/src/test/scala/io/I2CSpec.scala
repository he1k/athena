package io

import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
class I2CSpec extends AnyFlatSpec with ChiselScalatestTester {
  "I2C" should "pass" in {
    test(new I2CDriver).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { dut =>
      val steps = 75000
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.we.poke(false.B)
      dut.io.cmd.sla.poke("b1101000".U)
      dut.io.cmd.din.poke(0x69.U)
      dut.io.cmd.ra.poke(0x81.U)
      dut.clock.setTimeout(steps)
      dut.clock.step(steps)
    }
  }
}
