import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })
  val idle :: state :: pixelCheck :: write :: increment :: load :: done :: Nil = Enum (7)
  val stateReg = RegInit(idle)
  val registers = Reg(Vec (60, UInt (16.W)))

  val x = RegInit(0.U(6.W))
  val y = RegInit(0.U(6.W))

  val blackNeighbours = RegInit(false.B)

  //default values
  io.address := 0.U
  io.writeEnable := false.B
  io.done := false.B
  io.dataWrite := 0.U

  val index = RegInit(20.U(16.W))

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := state
      }
    }
    is(state) {
      when(index < 60.U) {
        io.address := index - 20.U
        registers(index) := io.dataRead
        index := index + 1.U
      }
      when (index === 60.U){
        stateReg := pixelCheck
      }
    }
    is(pixelCheck) {
      blackNeighbours := false.B
      when(((x === 0.U) || (y === 0.U) || (x === 19.U) || (y === 19.U)) || (registers(x) === 0.U)) {
        blackNeighbours := true.B
      }.otherwise{
          when(registers(x+19.U) === 0.U) {
            blackNeighbours := true.B
          }.elsewhen(registers(x+21.U) === 0.U){
              blackNeighbours := true.B
          }.elsewhen(registers(x) === 0.U){
              blackNeighbours := true.B
          }.elsewhen(registers(x+40.U) === 0.U){
              blackNeighbours := true.B
          }
      }
      stateReg := write
    }
    is(write) {
      io.writeEnable := true.B
      io.address := x+(y*20.U) + 400.U
      when(blackNeighbours) {
        io.dataWrite := 0.U
      }
        .otherwise {
          io.dataWrite := 255.U
        }
      stateReg := increment
    }
    is(increment) {
      when(y === 20.U) {
        stateReg := done
      }.elsewhen (x === 20.U) {
        x := 0.U
        y := y+1.U
        index := 0.U
        stateReg := load
      }.otherwise {
        x := x + 1.U
        stateReg := pixelCheck
      }
    }
    is(load) {
      when (index < 20.U){
        registers(index) := registers(index+20.U)
        registers(index+20.U) := registers(index+40.U)
        io.address := (y+1.U)*20.U + index
        registers(index+40.U) := io.dataRead
        index := index + 1.U
      } .otherwise{
        stateReg := pixelCheck
      }
    }
    is(done) {
      io.done := true.B
    }
  }
    //Write here your code
}
