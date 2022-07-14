package io
import chisel3._
import chisel3.util._
import utility.Constants._
class UART(size: Int) extends Module{
  val io = IO(new Bundle{
    val cmd = new Bundle{
      val ready = Output(Bool())
      val valid = Input(Bool())
      val din = Input(UInt(8.W))
      val dout = Output(UInt(8.W))
      val we = Input(Bool())
      val full = Output(Bool())
      val empty = Output(Bool())
    }
    val tx = Output(Bool())
    val rx = Input(Bool())
  })
  val uartTx = Module(new UARTTX)
  val uartRx = Module(new UARTRX)
  val fifoIn = Module(new FIFO(size))
  val fifoOut = Module(new FIFO(size))
  // Default fifo input
  fifoIn.io.din := uartRx.io.data
  fifoIn.io.we := false.B
  fifoIn.io.valid := false.B
  fifoOut.io.din := io.cmd.din
  fifoOut.io.we := false.B
  fifoOut.io.valid := false.B
  // Default uart input
  uartTx.io.valid := !fifoOut.io.empty
  uartTx.io.data := fifoOut.io.dout
  uartRx.io.ready := !fifoIn.io.full
  uartRx.io.rx := io.rx
  io.tx := uartTx.io.tx
  io.cmd.full := fifoOut.io.full
  io.cmd.empty := fifoIn.io.empty
  io.cmd.dout := fifoIn.io.dout
  io.cmd.ready := false.B
  when(uartRx.io.valid & !fifoIn.io.full){ // Prioritize receiving input
    fifoIn.io.we := true.B
    fifoIn.io.valid := true.B
  } .elsewhen(io.cmd.valid & !io.cmd.we & !fifoIn.io.empty){ // Reading input fifo
    io.cmd.ready := true.B
    fifoIn.io.we := false.B
    fifoIn.io.valid := true.B
  }
  when(uartTx.io.ready & !fifoOut.io.empty){ // Prioritize transmitting output
    fifoOut.io.we := false.B
    fifoOut.io.valid := true.B
  } .elsewhen(io.cmd.valid & io.cmd.we & !fifoOut.io.full){ // Write output fifo
    fifoOut.io.we := true.B
    fifoOut.io.valid := true.B
    io.cmd.ready := true.B
  }
}
class FIFO(size: Int) extends Module{
  val io = IO(new Bundle{
    val din = Input(UInt(8.W))
    val dout = Output(UInt(8.W))
    val empty = Output(Bool())
    val full = Output(Bool())
    val we = Input(Bool())
    val valid = Input(Bool())
  })

  val data = RegInit(0.U.asTypeOf(Vec(size, UInt(8.W))))
  val idx = RegInit(0.U(log2Up(size + 1).W))
  io.empty := idx === 0.U
  io.full := idx === size.U
  io.dout := data.head
  when(io.valid){
    when(io.we & !io.full){
      data(idx) := io.din
      idx := idx + 1.U
    } .elsewhen(!io.we & !io.empty){
      idx := idx - 1.U
      for(i <- 0 until size - 1){
        data(i) := data(i+1)
      }
      data(size - 1) := 0.U
    }
  }
}


class UARTTX extends Module{
  val io = IO(new Bundle{
    val tx = Output(Bool())
    val data = Input(UInt(8.W))
    val valid = Input(Bool())
    val ready = Output(Bool())
  })

  // The amount of counts / clock cycles one bit of data translates
  // counts pr. bit = clock frequency / baudrate
  val idle :: start :: process :: stop :: Nil = Enum(4)
  val stateReg = RegInit(idle)
  val cntReg = RegInit(0.U(10.W))
  val indexReg = RegInit(0.U(3.W))
  val bufferVec = Reg(Vec(8,UInt(1.W)))

  io.tx := true.B
  io.ready := stateReg === idle
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
class UARTRX extends Module{
  val io = IO(new Bundle{
    val rx = Input(Bool())
    val data = Output(UInt(8.W))
    val ready = Input(Bool())
    val valid = Output(Bool())
  })
  // The amount of counts / clock cycles one bit of data translates
  // counts pr. bit = clock frequency / baudrate
  val idle :: start :: process :: stop :: Nil = Enum(4)
  val stateReg = RegInit(idle)
  val cntReg = RegInit(0.U(10.W))
  val indexReg = RegInit(0.U(4.W))
  val bufferVec = Reg(Vec(8,UInt(1.W)))
  val validReg = Reg(Bool())
  val rxReg = RegNext(RegNext(io.rx, true.B), true.B)
  val shiftReg = RegInit(0.U(8.W))

  // Could be structured better, with idle not checking for start bit.
  // Maybe just go from idle to start whenever rx line is pulled low.
  // Then in start check if this was indeed the stop bit, and then sample
  // the line in process state
  io.valid := false.B
  switch(stateReg){
    is(idle){
      cntReg := 0.U
      indexReg := 0.U
      // Are we recieving start bit
      when(!rxReg & io.ready){
        cntReg := cntReg + 1.U
        // Checking if we are in the middle of the start bit
        when(cntReg === (CPB / 2).U){
          stateReg := start
          cntReg := 0.U
        }
      }
    }
    is(start){
      cntReg := cntReg + 1.U
      when(cntReg === CPB.U){
        cntReg := 0.U
        bufferVec(indexReg) := rxReg
        indexReg := indexReg + 1.U
      }
      when(indexReg === 8.U) {
        indexReg := 0.U
        stateReg := stop
      }
    }
    is(stop){
      // Counting one more bit to reach stop bit, and checking if line has been pulled high again.
      cntReg := cntReg + 1.U
      when(cntReg === CPB.U){
        stateReg := idle
        io.valid := true.B
      }
    }
  }
  io.data := bufferVec(7) ## bufferVec(6) ## bufferVec(5) ## bufferVec(4) ## bufferVec(3) ## bufferVec(2) ##
    bufferVec(1) ## bufferVec(0)
}
