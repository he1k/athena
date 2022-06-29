package arithmetic
import chisel3._
import InvLookUp._
import scala.math.pow

object InvLookUp{
  def binToFP(x: Int): Float={
    var dec = 0F
    for(i <- 0 to 23){
      dec = dec + ((x >>> i) & 0x00000001)*pow(2, -(23-i)).toFloat
    }
    dec
  }
  def FPDiv(x: BigInt, y: BigInt): BigInt={
    (x << 23)/y
  }
  def invLUT(width: Int = 5): Array[Int]={
    val a: BigInt = 0x800000
    var b: BigInt = 0x800000
    val max = (pow(2,width)-1).toInt
    val arr = Array.ofDim[Int](max + 1)
    for(i <- 0 to max){
      b = 0x800000 + (i << (23-width))
      arr(i) = FPDiv(a,b).toInt
    }
    arr
  }
}
class InvLookUp(width: Int = 5) extends Module{
  val io = IO(new Bundle{
    val mant = Input(UInt(width.W))
    val invMant = Output(UInt(24.W))
  })
  val arr: Array[Int] = invLUT(width)
  val iLUT = VecInit(arr.map(_.S(32.W)))
  io.invMant := iLUT(io.mant(4,0)).asUInt
}