import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
class I2CSpec extends AnyFlatSpec with ChiselScalatestTester {

  "I2C" should "pass" in { // , VerilatorBackendAnnotation
    test(new I2C).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { dut =>
      val steps = 7500
      dut.clock.setTimeout(steps)
      dut.clock.step(steps)
    }
  }
}
