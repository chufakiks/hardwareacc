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
  val registers = Vec(60, RegInit(0.U(16.W)))
  var x = 0.U
  var y = 0.U
  var blackNeighbours = false.B

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        var count = 20.U
        when(count < 59.U) {
          io.address := count - 20.U
          registers(count) == io.dataRead
          count = count + 1.U
        }
      }
        .otherwise{
          stateReg := pixelCheck
        }
    }
    is(pixelCheck) {
      val borderPixel = (x === 0.U) || (y === 0.U) || (x === 19.U) || (y === 19.U)
      val blackPixel = registers(x+y*20) === 0.U
      when(borderPixel || blackPixel) {
        blackNeighbours = true.B
      }
        .otherwise{
          val leftNeighbour = (x-1.U)+y*20
          val rightNeighbour = (x+1.U)+y*20
          val upperNeighbour = (x+(y-1.U)*20.U)
          val bottomNeighbour = (x+(y+1.U)*20.U)
          when(registers(leftNeighbour) === 0.U) {
            blackNeighbours = true.B
          }
            .elsewhen(registers(rightNeighbour) === 0.U){
              blackNeighbours = true.B
            }
            .elsewhen(registers(rightNeighbour) === 0.U){
              blackNeighbours = true.B
            }
            .elsewhen(registers(bottomNeighbour) === 0.U){
              blackNeighbours = true.B
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
        x=0.U
        y=y+1.U
      }
        .otherwise {
          x = x + 1.U
        }
      when(y > 19.U) {
        stateReg := done
      }
        .otherwise{
          stateReg := load
        }
    }
    is(load) {
      var i = y*20.U
      when(i < y * 20.U + 20.U) {
        val reg1 = registers(i + 20.U)
        registers(i) == reg1
      }
        .elsewhen(i < y * 20.U + 40.U) {
          val reg2 = registers(i + 2*20.U) // finished here <3
          registers(i) == reg2

        }
        .otherwise {
          val reg3 = io.dataRead

        }
    }
    is(done) {
      io.done := true.B
    }
  }
    //Write here your code
}
