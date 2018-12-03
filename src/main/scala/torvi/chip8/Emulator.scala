package torvi.chip8

import java.nio.file.{Files, Paths}

class Emulator {
  val memory: Array[Byte] = Array.fill[Byte](4096)(0)
  val stack: Array[Short] = Array.fill[Short](16)(0)
  val registers: Array[Byte] = Array.fill[Byte](16)(0)
  val addressRegister: Array[Short] = Array.fill[Short](1)(0)
  val keyboardInput: Array[Boolean]  = Array.fill[Boolean](16)(false)
  var stackPointer: Short = 0
  var programCounter: Short = 0x200
  var soundTimer: Short = 0
  var delayTimer: Short = 0

  def loadRom(fPath: String) = {
    val rom = Files.readAllBytes(Paths.get(fPath))
    if(rom.length > (memory.length - 512)) throw new RomTooLargeException("Rom is too large")

    for ((b: Byte, idx: Int) <- rom.zipWithIndex) { memory(512 + idx) = b }
  }

}
