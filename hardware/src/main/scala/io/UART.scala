package io
import chisel3._
import chisel3.util._
import utility.Constants._
class UART {

}


class UARTTX extends Module{
  val io = IO(new Bundle{
    val tx = Output(Bool())
    val data = Input(UInt(8.W))
    val valid = Input(Bool())
  })

  // The amount of counts / clock cycles one bit of data translates
  // counts pr. bit = clock frequency / baudrate
  val idle :: start :: process :: stop :: Nil = Enum(4)
  val stateReg = RegInit(idle)
  val cntReg = RegInit(0.U(10.W))
  val indexReg = RegInit(0.U(3.W))
  val bufferVec = Reg(Vec(8,UInt(1.W)))

  io.tx := true.B
  switch(stateReg){
    is(idle){
      cntReg := 0.U
      indexReg := 0.U
      when(io.valid){
        stateReg := start
        for(i <- 0 to 7){
          bufferVec(i.U) := io.data(i.U)
        }
      }
    }
    is(start){
      io.tx := false.B
      cntReg := cntReg + 1.U
      when(cntReg === (CPB - 1).U){
        stateReg := process
        cntReg := 0.U
      }
    }
    is(process){
      cntReg := cntReg + 1.U
      io.tx := bufferVec(indexReg)
      when(cntReg === (CPB - 1).U){
        cntReg := 0.U
        indexReg := indexReg + 1.U
        when(indexReg === 7.U){
          stateReg := stop
        }
      }
    }
    is(stop){
      cntReg := cntReg + 1.U
      io.tx := true.B
      when(cntReg === (CPB - 1).U){
        stateReg := idle
      }
    }
  }
}
