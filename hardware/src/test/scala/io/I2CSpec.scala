package io

import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
class I2CSpec extends AnyFlatSpec with ChiselScalatestTester {

  "I2C" should "pass" in { // , VerilatorBackendAnnotation
    test(new I2C).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { dut =>
      val steps = 20000
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.din.poke("b1101000".U)
      dut.clock.setTimeout(steps)
      dut.clock.step(steps)
    }
  }
}
