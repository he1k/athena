package arithmetic

import chisel3._
import chisel3.util._
class Stage1Add extends Bundle{
  val e = UInt(8.W)
  val shifts = UInt(9.W)
  val ltb = Bool() // is a exponent less than b exponent
  val m = new Bundle{
    val a = UInt((27).W)
    val b = UInt((27).W)
  }
  val s = new Bundle{
    val a = Bool()
    val b = Bool()
  }
}
class Stage2Add extends Bundle{
  val e = UInt(8.W)
  val m = new Bundle{
    val a = UInt((28).W)
    val b = UInt((28).W)
  }
  val s = new Bundle{
    val a = Bool()
    val b = Bool()
  }
  val sticky = Bool()
}
class Stage3Add extends Bundle{
  val e = UInt(8.W)
  val m = UInt(28.W)
  val s = Bool()
  val sticky = Bool()
}
class Stage4Add extends Bundle{
  val e = UInt(8.W)
  val m = UInt(27.W)
  val s = Bool()
}
class FPAdd extends Module{
  val io = IO(new Bundle{
    val a, b = Input(UInt(32.W))
    val sub, en = Input(Bool())
    val y = Output(UInt(36.W))
  })
  //-------------------------------------------------------------//
  // Stage 1
  // -> Find exponent difference
  val e_a = io.a(30, 23)
  val m_a = io.a(22, 0)
  val e_b = io.b(30, 23)
  val m_b = io.b(22, 0)
  val stage1 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val shifts = UInt(9.W)
    val ltb = Bool() // is a exponent less than b exponent
    val m = new Bundle{
      val a = UInt((27).W)
      val b = UInt((27).W)
    }
    val s = new Bundle{
      val a = Bool()
      val b = Bool()
    }
  }))
  val diff = Wire(SInt(9.W))
  diff := (e_a - e_b).asSInt
  when(io.en) {
    stage1.shifts := (~(diff)).asUInt + 1.U
    stage1.m.a := Mux(e_a === 0.U, 0.U ## m_a, 1.U ## m_a) ## 0.U(3.W) // Check if operand is denormalized and add extra bit for precision
    stage1.m.b := Mux(e_b === 0.U, 0.U ## m_b, 1.U ## m_b) ## 0.U(3.W)
    stage1.e := Mux(diff(8), e_b, e_a)
    stage1.shifts := Mux(diff(8), (~(diff)).asUInt + 1.U, diff.asUInt)
    stage1.ltb := diff(8)
    stage1.s.a := io.a(31)
    stage1.s.b := Mux(io.sub, ~io.b(31), io.b(31))
  }
  //-------------------------------------------------------------//
  // Stage 2
  // -> Shift mantissa
  val stage2 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = new Bundle{
      val a = UInt((28).W)
      val b = UInt((28).W)
    }
    val s = new Bundle{
      val a = Bool()
      val b = Bool()
    }
    val sticky = Bool()
  }))
  val stickyDist = PriorityEncoder(Mux(stage1.ltb, stage1.m.a, stage1.m.b)) // Find distance to first 1 in the lesser mantissa
  // This distance must be less than or equal
  when(io.en) {
    stage2.m.a := Mux(stage1.ltb, stage1.m.a >> stage1.shifts, stage1.m.a)
    stage2.m.b := Mux(stage1.ltb, stage1.m.b, stage1.m.b >> stage1.shifts)
    stage2.e := stage1.e
    stage2.s.a := stage1.s.a
    stage2.s.b := stage1.s.b
    stage2.sticky := (stickyDist <= stage1.shifts) & (stage1.shifts >= 3.U)
  }

  //-------------------------------------------------------------//
  // Stage 3
  // -> Add mantissas
  val stage3 = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt(28.W)
    val s = Bool()
    val sticky = Bool()
  }))
  val m_a_wide = WireDefault(0.S((29).W))
  val m_b_wide = WireDefault(0.S((29).W))
  m_a_wide := Mux(stage2.s.a, (-stage2.m.a).asSInt, (stage2.m.a).asSInt)
  m_b_wide := Mux(stage2.s.b, (-stage2.m.b).asSInt, (stage2.m.b).asSInt)
  val m_sum = WireDefault(0.S((30).W))
  m_sum := m_a_wide + m_b_wide
  when(io.en){
    stage3.e := stage2.e
    stage3.m := Mux(m_sum(29),(-m_sum).asUInt ,m_sum.asUInt)
    stage3.s := m_sum(29)
    stage3.sticky := stage2.sticky
  }

  //-------------------------------------------------------------//
  // Stage 4 + 5
  // -> Normalize
  val unorm = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt(27.W)
    val s = Bool()
    val sticky = Bool()
    val distUpper = UInt(8.W)
    val distLower = UInt(8.W)
  }))
  when(io.en){
    unorm.e := stage3.e
    unorm.m := stage3.m
    unorm.s := stage3.s
    unorm.sticky := stage3.sticky
    unorm.distUpper := PriorityEncoder(Reverse(stage3.m)) // To determine how much we need to shift mantissa
    unorm.distLower := PriorityEncoder(stage3.m) // Distance from sticky bit to first set bit of mantissa
  }
  val norm = RegInit(0.U.asTypeOf(new Bundle{
    val e = UInt(8.W)
    val m = UInt(27.W)
    val s = Bool()
  }))

  val zero = unorm.m === 0.U
  val distExpct = 1.U//(3-precision).U // First set bit should be 3 away from MSB of input.
  val shiftleft = unorm.distUpper > distExpct
  val shiftright = unorm.distUpper < distExpct
  val shifts = Mux(shiftleft,(unorm.distUpper - distExpct), (distExpct - unorm.distUpper)).asUInt
  val setS = (unorm.distLower <= shifts) | unorm.m(0) | unorm.sticky
  when(io.en){
    norm.m := unorm.m
    norm.e := unorm.e
    norm.s := unorm.s
    when(!zero){
      when(shiftleft){
        norm.m := unorm.m << shifts
        norm.e := unorm.e - shifts
      } . elsewhen(shiftright){
        norm.m := unorm.m >> shifts
        norm.e := unorm.e + shifts
      }
    }
    when(zero){
      norm.e := 0.U
    }
  }
  //-------------------------------------------------------------//
  // Stage 6
  // -> Output non-rounded result
  io.y := norm.s ## norm.e ## norm.m
}
