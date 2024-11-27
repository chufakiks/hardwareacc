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
  val idle :: pixelCheck :: write :: increment :: load :: done :: Nil = Enum (6)
  val stateReg = RegInit(idle)
  val registers = Reg(Vec (60, UInt (16.W)))
  //var x = 0.U
  //var y = 0.U
  //var index = 0.U
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))


  val blackNeighbours = RegInit(false.B)

  //default values
  io.address := 0.U
  io.writeEnable := false.B
  io.done := false.B
  io.dataWrite := 0.U

  val index = RegInit(20.U(6.W))

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        when(index < 60.U) {
          io.address := index - 20.U
          registers(index) := io.dataRead
          index := index + 1.U
          x := index + 2.U
          //printf(p"[IDLE] Address: ${io.address}, Count: $index\n")
          stateReg := idle
        }.otherwise{
          stateReg := pixelCheck
        }
      }
    }
    is(pixelCheck) {
      val borderPixel = (x === 0.U) || (y === 0.U) || (x === 19.U) || (y === 19.U)
      val blackPixel = registers(x+y*20.U) === 0.U
      when(borderPixel || blackPixel) {
        blackNeighbours := true.B
      }
        .otherwise{
          val leftNeighbour = (x-1.U)+y*20.U
          val rightNeighbour = (x+1.U)+y*20.U
          val upperNeighbour = x+(y-1.U)*20.U
          val bottomNeighbour = x+(y+1.U)*20.U
          when(registers(leftNeighbour) === 0.U) {
            blackNeighbours := true.B
          }
            .elsewhen(registers(rightNeighbour) === 0.U){
              blackNeighbours := true.B
            }
            .elsewhen(registers(upperNeighbour) === 0.U){
              blackNeighbours := true.B
            }
            .elsewhen(registers(bottomNeighbour) === 0.U){
              blackNeighbours := true.B
            }

        }
        stateReg := write
    }
    is(write) {
      io.address := x+y*20.U
      io.writeEnable := true.B
      when(blackNeighbours) {
        io.dataWrite := 0.U
      }
        .otherwise {
          io.dataWrite := 255.U
        }
      stateReg := increment
    }
    is(increment) {
      when(x>=19.U) {
        x := 0.U
        y := y+1.U
      }
        .otherwise {
          x := x + 1.U // hej
        }
      when(y > 19.U) {
        stateReg := done
      }
        .otherwise{
          index := 0.U
          stateReg := load
        }
    }
    is(load) {
      when (index < 20.U){
        registers(index) := registers(index+20.U)
        registers(index+20.U) := registers(index+40.U)
        io.address := (y+1.U)*20.U + index
        registers(index+40.U) := io.dataRead
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
