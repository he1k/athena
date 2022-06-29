package arithmetic
import chisel3._
import chisel3.util._

class Mul(width: Int) extends Module{
  val io = IO(new Bundle{
    val a, b = Input(UInt(width.W))
    val y = Output(UInt((2*width).W))
    val en = Input(Bool())
  })
  // Stage 1 generate partial products
  val p1 = RegInit(io.a * io.b(7,0))
  val p2 = RegInit(io.a * (io.b(15, 8) ## 0.U(8.W)))
  val p3 = RegInit(io.a * (io.b(23,16) ## 0.U(16.W)))
  val p4 = RegInit(io.a * (io.b(width-1,24) ## 0.U(24.W)))
  // Stage 2 summation
  val parsum1 = RegInit(p1 +& p2)
  val parsum2 = RegInit(p3 +& p4)
  // stage 3 summation
  val sum = RegInit(parsum1 + parsum2)
  // stage 4 output
  io.y := sum
  when(io.en){
    p1 := io.a * io.b(7,0)
    p2 := io.a * (io.b(15, 8) ## 0.U(8.W))
    p3 := io.a * (io.b(23,16) ## 0.U(16.W))
    p4 := io.a * (io.b(width-1,24) ## 0.U(24.W))
    parsum1 := p1 +& p2
    parsum2 := p3 +& p4
    sum := parsum1 + parsum2

  }
}
class DivIterationStage(width: Int, shifts: Int, dual: Boolean) extends Module{
  val io = IO(new Bundle{
    val in = Input(new Bundle{
      val a, b, c = Input(UInt(width.W))
      val e = Input(UInt(8.W))
      val s = Bool()
    })
    val out = Output(new Bundle{
      val a, b = Input(UInt(width.W))
      val e = Input(UInt(8.W))
      val s = Bool()
    })
    val en = Input(Bool())
  })
  var mul = Seq(Module(new Mul(width)))
  if(dual){
    mul = mul :+ Module(new Mul(width))
  }
  val e = RegInit(0.U.asTypeOf(Vec(3, UInt(8.W))))
  val s = RegInit(0.U.asTypeOf(Vec(3, Bool())))
  when(io.en){
    e(0) := io.in.e
    s(0) := io.in.s
    for(i <- 1 until 3){
      e(i) := e(i-1)
      s(i) := s(i-1)
    }
  }
  mul(0).io.a := io.in.a
  mul(0).io.b := io.in.c
  if(dual){
    mul(1).io.a := io.in.b
    mul(1).io.b := io.in.c
  }
  mul(0).io.en := io.en
  if(dual){
    mul(1).io.en := io.en
  }
  io.out.a := mul(0).io.y >> shifts
  io.out.b := (if(dual){mul(1).io.y >> shifts}else{0.U})
  io.out.e := e(2)
  io.out.s := s(2)

}
class FPDiv(width: Int = 5) extends Module{
  val io = IO(new Bundle{
    val a, b = Input(SInt(32.W))
    val y = Output(UInt(36.W))
    val en = Input(Bool())
  })
  //------------------------------------------------------------//
  // Stage 1
  // Calculate sign and exponent
  val stage1 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = new Bundle{
      val a = UInt(24.W)
      val b = UInt(24.W)
    }
    val s = Bool()
  }))
  when(io.en){
    stage1.s := io.a(31) ^ io.b(31)
    stage1.e := (io.a(30, 23) - io.b(30, 23)) + 127.U
    stage1.m.a := 1.U ## io.a(22, 0)
    stage1.m.b := 1.U ## io.b(22, 0)
  }


  //-----------------------------------------------------------//
  // Constants
  val guard = 8
  val distExpct = 1.U
  val fixedTwo = (0x1000000 << guard).U

  //-----------------------------------------------------------//
  // Global enable
  val en = io.en

  //-----------------------------------------------------------//
  // Stage 2
  // Get approx 1/b
  val stage2 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = new Bundle{
      val a = UInt((24+guard).W)
      val b = UInt((24+guard).W)
      val appr = UInt((24+guard).W)
    }
    val s = Bool()
  }))

  val invLUT = Module(new InvLookUp(width))
  invLUT.io.mant := stage1.m.b(22, 22 -(width-1))
  when(io.en){
    stage2.e := stage1.e
    stage2.s := stage1.s
    stage2.m.a := stage1.m.a ## 0.U(guard.W)
    stage2.m.b := stage1.m.b ## 0.U(guard.W)
    stage2.m.appr := invLUT.io.invMant ## 0.U(guard.W)
  }

  //-----------------------------------------------------------//
  // Stage 3
  // Multiply with approx 1/b
  val stage3 = Module(new DivIterationStage(24 + guard, 23 + guard, true))
  stage3.io.in.a := stage2.m.a
  stage3.io.in.b := stage2.m.b
  stage3.io.in.c := stage2.m.appr
  stage3.io.in.e := stage2.e
  stage3.io.in.s := stage2.s
  stage3.io.en := en


  //-----------------------------------------------------------//
  // Stage 4
  // Iterate
  val stage4 = Module(new DivIterationStage(24 + guard, 23 + guard, true))
  stage4.io.in.a := stage3.io.out.a
  stage4.io.in.b := stage3.io.out.b
  stage4.io.in.c := (fixedTwo - stage3.io.out.b)
  stage4.io.in.e := stage3.io.out.e
  stage4.io.in.s := stage3.io.out.s
  stage4.io.en := en

  //-----------------------------------------------------------//
  // Stage 5
  // Iterate
  val stage5 = Module(new DivIterationStage(24 + guard, 23 + guard, true))
  stage5.io.in.a := stage4.io.out.a
  stage5.io.in.b := stage4.io.out.b
  stage5.io.in.c := (fixedTwo - stage4.io.out.b)
  stage5.io.in.e := stage4.io.out.e
  stage5.io.in.s := stage4.io.out.s
  stage5.io.en := en
  //-----------------------------------------------------------//
  // Stage 6
  // Iterate
  val stage6 = Module(new DivIterationStage(25 + guard, 23 + guard, false))
  stage6.io.in.a := stage5.io.out.a
  stage6.io.in.b := stage5.io.out.b
  stage6.io.in.c := (fixedTwo - stage5.io.out.b)
  stage6.io.in.e := stage5.io.out.e
  stage6.io.in.s := stage5.io.out.s
  stage6.io.en := en


  //-----------------------------------------------------------//
  // Stage 7
  // Normalize
  val unorm = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt((25+guard).W)
    val s = Bool()
    val dist = UInt(5.W)
  }))
  when(io.en){
    unorm.e := stage6.io.out.e
    unorm.s := stage6.io.out.s
    unorm.m := stage6.io.out.a
    unorm.dist := PriorityEncoder(Reverse(stage6.io.out.a))
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
