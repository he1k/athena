package io
import chisel3._
import chisel3.util._
import utility.Constants._
class MotorDrive(channels: Int) extends Module{
  val io = IO(new Bundle{
    val ch = Vec(channels, new Bundle{
      val cmd = new Bundle{
        val set = Input(Bool())
        val max = Input(UInt(log2Up(PWM_MAX_CNT).W))
        val dir = Input(Bool())
        val ena = Input(Bool())
        val enca = Output(Bool())
        val encb = Output(Bool())
      }
      val motor = new Bundle{
        val enca = Input(Bool())
        val encb = Input(Bool())
        val in1 = Output(Bool())
        val in2 = Output(Bool())
        val pwm = Output(Bool())
      }
    })
  })
  val pwm = Module(new PWM(channels))
  for(i <- 0 until channels){
    pwm.io.ch(i).set := io.ch(i).cmd.set
    pwm.io.ch(i).max := io.ch(i).cmd.max
    when(io.ch(i).cmd.ena){
      io.ch(i).motor.in1 := !io.ch(i).cmd.dir
      io.ch(i).motor.in2 := io.ch(i).cmd.dir
    }.otherwise{ // Turn off motor
      io.ch(i).motor.in1 := false.B
      io.ch(i).motor.in2 := false.B
    }
    io.ch(i).motor.pwm := pwm.io.ch(i).pwm
    io.ch(i).cmd.enca := RegNext(RegNext(io.ch(i).motor.enca)) // Maybe the motor driver can also look for rising edge
    io.ch(i).cmd.encb := RegNext(RegNext(io.ch(i).motor.encb)) // and corresponding direction.
  }
}
