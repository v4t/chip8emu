package torvi.chip8

import scala.collection.mutable.Stack

class Emulator {
  val memory: Array[Byte] = Array.fill[Byte](4096)(0)
  val stack = Stack[Short]()
  val registers: Array[Byte] = Array.fill[Byte](16)(0)
  val keyboardInput: Array[Boolean] = Array.fill[Boolean](16)(false)
  var addressRegister: Short = 0
  var stackPointer: Short = 0
  var programCounter: Short = 0x200
  var soundTimer: Byte = 0
  var delayTimer: Byte = 0
  var drawFlag = true

  val spriteStartAddr: Byte = 0
  val spriteLength: Byte = 5

  def loadRom(rom: Array[Byte]) = {
    if (rom.length > (memory.length - 512)) throw new RomTooLargeException("Rom is too large")

    for ((b: Byte, idx: Int) <- rom.zipWithIndex) {
      memory(512 + idx) = b
    }
  }

  def executeCycle() = {
    val opCode = ((memory(programCounter) << 8) | (memory(programCounter + 1) & 0x00FF)).toShort
    opCode & 0xF000 match {
      case 0x0000 =>
        opCode & 0x00ff match {
          case 0x00e0 => Instruction.clearScreen(this, opCode)
          case 0x00ee => Instruction.returnFromSub(this, opCode)
        }
      case 0x1000 => Instruction.jumpToAddr(this, opCode)
      case 0x2000 => Instruction.callSub(this, opCode)
      case 0x3000 => Instruction.skipVxEqualsNn(this, opCode)
      case 0x4000 => Instruction.skipVxNotEqualsNn(this, opCode)
      case 0x5000 => Instruction.skipVxEqualsVy(this, opCode)
      case 0x6000 => Instruction.setVxToNn(this, opCode)
      case 0x7000 => Instruction.addNnToVx(this, opCode)
      case 0x8000 =>
        opCode & 0xf00f match {
          case 0x8000 => Instruction.setVxToVy(this, opCode)
          case 0x8001 => Instruction.setVxToVxOrVy(this, opCode)
          case 0x8002 => Instruction.setVxToVxAndVy(this, opCode)
          case 0x8003 => Instruction.setVxToVxXorVy(this, opCode)
          case 0x8004 => Instruction.addVyToVx(this, opCode)
          case 0x8005 => Instruction.subVyFromVx(this, opCode)
          case 0x8006 => Instruction.shiftRightVx(this, opCode)
          case 0x8007 => Instruction.setVxToVyMinusVx(this, opCode)
          case 0x800e => Instruction.shiftLeftVx(this, opCode)
        }
      case 0x9000 => Instruction.skipVxNotEqualsVy(this, opCode)
      case 0xa000 => Instruction.setAddrRegToNn(this, opCode)
      case 0xb000 => Instruction.jumpToAddrPlusV0(this, opCode)
      case 0xc000 => Instruction.setVxToRand(this, opCode)
      case 0xd000 => Instruction.drawVxVyN(this, opCode)
      case 0xe000 =>
        opCode & 0xf0ff match {
          case 0xe09e => Instruction.skipVxPressed(this, opCode)
          case 0xe0a1 => Instruction.skipVxNotPressed(this, opCode)
        }
      case 0xf000 =>
        opCode & 0xf0ff match {
          case 0xf007 => Instruction.setVxToDelayTimerValue(this, opCode)
          case 0xf00a => Instruction.waitForKeyPress(this, opCode)
          case 0xf015 => Instruction.setDelayTimerValueToVx(this, opCode)
          case 0xf018 => Instruction.setSoundTimerValueToVx(this, opCode)
          case 0xf01e => Instruction.addVxToAddrReg(this, opCode)
          case 0xf029 => Instruction.setAddrRegToSpriteAddr(this, opCode)
          case 0xf033 => Instruction.storeBcdForVx(this, opCode)
          case 0xf055 => Instruction.regDump(this, opCode)
          case 0xf065 => Instruction.regLoad(this, opCode)
        }
    }
  }
}
