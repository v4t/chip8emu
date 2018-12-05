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

  initFontSet()

  private def initFontSet(): Unit = {
    val fontSet: Array[Byte] = Array(
      0xF0.toByte, 0x90.toByte, 0x90.toByte, 0x90.toByte, 0xF0.toByte, //0
      0x20.toByte, 0x60.toByte, 0x20.toByte, 0x20.toByte, 0x70.toByte, //1
      0xF0.toByte, 0x10.toByte, 0xF0.toByte, 0x80.toByte, 0xF0.toByte, //2
      0xF0.toByte, 0x10.toByte, 0xF0.toByte, 0x10.toByte, 0xF0.toByte, //3
      0x90.toByte, 0x90.toByte, 0xF0.toByte, 0x10.toByte, 0x10.toByte, //4
      0xF0.toByte, 0x80.toByte, 0xF0.toByte, 0x10.toByte, 0xF0.toByte, //5
      0xF0.toByte, 0x80.toByte, 0xF0.toByte, 0x90.toByte, 0xF0.toByte, //6
      0xF0.toByte, 0x10.toByte, 0x20.toByte, 0x40.toByte, 0x40.toByte, //7
      0xF0.toByte, 0x90.toByte, 0xF0.toByte, 0x90.toByte, 0xF0.toByte, //8
      0xF0.toByte, 0x90.toByte, 0xF0.toByte, 0x10.toByte, 0xF0.toByte, //9
      0xF0.toByte, 0x90.toByte, 0xF0.toByte, 0x90.toByte, 0x90.toByte, //A
      0xE0.toByte, 0x90.toByte, 0xE0.toByte, 0x90.toByte, 0xE0.toByte, //B
      0xF0.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0xF0.toByte, //C
      0xE0.toByte, 0x90.toByte, 0x90.toByte, 0x90.toByte, 0xE0.toByte, //D
      0xF0.toByte, 0x80.toByte, 0xF0.toByte, 0x80.toByte, 0xF0.toByte, //E
      0xF0.toByte, 0x80.toByte, 0xF0.toByte, 0x80.toByte, 0x80.toByte  //F
    )
    for ((b: Byte, idx: Int) <- fontSet.zipWithIndex) {
      memory(idx) = b
    }
  }
  
  def loadRom(rom: Array[Byte]): Unit = {
    if (rom.length > (memory.length - 512)) throw new RomTooLargeException("Rom is too large")

    for ((b: Byte, idx: Int) <- rom.zipWithIndex) {
      memory(512 + idx) = b
    }
  }

  def executeCycle(): Unit = {
    val opCode = ((memory(programCounter) << 8) | (memory(programCounter + 1) & 0x00FF)).toShort
    opCode & 0xf000 match {
      case 0x0000 =>
        opCode & 0x00ff match {
          case 0x00e0 => Instruction.clearScreen(this, opCode)
          case 0x00ee => Instruction.returnFromSub(this, opCode)
          case _ => throw new InvalidOpCodeException("Invalid opcode: " + opCode)
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
          case _ => throw new InvalidOpCodeException("Invalid opcode: " + opCode)
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
          case _ => throw new InvalidOpCodeException("Invalid opcode: " + opCode)
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
          case _ => throw new InvalidOpCodeException("Invalid opcode: " + opCode)
        }
      case _ => throw new InvalidOpCodeException("Invalid opcode: " + opCode)
    }
    updateDelayTimer()
    updateSoundTimer()
  }

  def updateDelayTimer(): Unit = {
    val intTimer = delayTimer & 0xff
    if (intTimer > 0) delayTimer = (intTimer - 1).toByte
  }

  def updateSoundTimer(): Unit = {
    val intTimer = soundTimer & 0xff
    if (intTimer > 0) {
      if (intTimer == 1) println("beep")
      soundTimer = (intTimer - 1).toByte
    }
  }
}
