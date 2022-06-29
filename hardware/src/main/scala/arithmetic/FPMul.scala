package arithmetic
import chisel3._
import chisel3.util._
class FPMul extends Module{
  val io = IO(new Bundle{
    val a, b = Input(UInt(32.W))
    val y = Output(UInt(36.W))
    val en = Input(Bool())
  })
  val guard = 5
  val en = io.en
  val distExpct = 1.U

  //------------------------------------------------------------//
  // Stage 1
  // Calculate sign and exponent
  val stage1 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val s = Bool()
    val m = new Bundle{
      val a = UInt((24 + guard).W)
      val b = UInt((24 + guard).W)
    }
  }))
  when(en){
    stage1.s := io.a(31) ^ io.b(31)
    stage1.e := (io.a(30, 23) + io.b(30, 23)) - 127.U
    stage1.m.a := 1.U ## io.a(22, 0) ## 0.U(guard.W)
    stage1.m.b := 1.U ## io.b(22, 0) ## 0.U(guard.W)
  }
  //------------------------------------------------------------//
  // Stage 2
  // Multiply mantissas
  val e = RegInit(0.U.asTypeOf(Vec(3, UInt(8.W))))
  val s = RegInit(0.U.asTypeOf(Vec(3, Bool())))
  val mul = Module(new Mul(24 + guard))
  mul.io.en := en
  mul.io.a := stage1.m.a
  mul.io.b := stage1.m.b
  when(en){
    e(0) := stage1.e
    s(0) := stage1.s
    for(i <- 1 until 3){
      e(i) := e(i-1)
      s(i) := s(i-1)
    }
  }
  //------------------------------------------------------------//
  // Stage 3
  // Normalize
  val unorm = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt((25+guard).W)
    val s = Bool()
    val dist = UInt(5.W)
  }))
  when(en){
    unorm.e := e(2)
    unorm.s := s(2)
    unorm.m := mul.io.y >> (23 + guard)
    unorm.dist := PriorityEncoder(Reverse((mul.io.y >> (23 + guard)).asUInt))
  }
  val norm = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt(27.W)
    val s = Bool()
    val sticky = Bool()
  }))
  when(io.en){
    norm.m := unorm.m(23 + guard, guard - 3)
    norm.e := unorm.e
    norm.s := unorm.s
    norm.sticky := (unorm.m(guard - 3, 0) =/= 0.U)
    when(unorm.dist > distExpct){
      norm.m := (unorm.m << (unorm.dist - distExpct))(23 + guard, guard - 3)
      norm.e := unorm.e - (unorm.dist - distExpct)
    } . elsewhen(unorm.dist < distExpct){
      norm.m := (unorm.m >> (distExpct - unorm.dist))(23 + guard, guard - 3)
      norm.e := unorm.e + (distExpct - unorm.dist)
    }
  }
  io.y :=  norm.s ## norm.e ## (norm.m(26,1) ## (norm.m(0) | norm.sticky))
}
