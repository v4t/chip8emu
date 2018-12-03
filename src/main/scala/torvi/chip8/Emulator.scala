package torvi.chip8

class Emulator(val memory: Array[Byte],
               val registers: Array[Byte],
               val addressRegister: Array[Short],
               val stack: Array[Short],
               val keyboardInput: Array[Boolean]) {
  var stackPointer: Short = 0
  var programCounter: Short = 0
  var soundTimer: Short = 0
  var delayTimer: Short = 0
}

object Emulator {

  def initialize(memory: Array[Byte],
                registers: Array[Byte],
                addressRegister: Array[Short],
                stack: Array[Short],
                keyboardInput: Array[Boolean]): Emulator =
    new Emulator(memory, registers, addressRegister, stack, keyboardInput)

  def loadRom(fPath: String) = println("TODO")

}
