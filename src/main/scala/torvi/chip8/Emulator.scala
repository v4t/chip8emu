package torvi.chip8

class Emulator {
  val memory: Array[Byte] = Array.fill[Byte](4096)(0)
  val registers: Array[Byte] = Array.fill[Byte](16)(0)
  val stack: Array[Int] = Array.fill[Int](16)(0)
  val keyboardInput: Array[Boolean] = Array.fill[Boolean](16)(false)
  var addressRegister: Int = 0
  var stackPointer: Int = 0
  var programCounter: Int = 0x200
  var soundTimer: Int = 0
  var delayTimer: Int = 0
  val spriteStartAddr: Int = 0
  val spriteLength: Int = 5
  val screenWidth = 64
  val screenHeight = 32
  val screenPixels: Array[Boolean] = Array.fill[Boolean](screenWidth * screenHeight)(false)

  initFontSet()

  def loadRom(rom: Array[Byte]): Unit = {
    if (rom.length > (memory.length - 512)) throw new RomTooLargeException("Rom is too large")

    for ((b: Byte, idx: Int) <- rom.zipWithIndex) {
      memory(512 + idx) = b
    }
  }

  def executeCycle(): Unit = {
    val pc = getProgramCounter
    val opCode = (getMemoryAt(pc) << 8) | getMemoryAt(pc + 1)
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
    val timer = getDelayTimer
    if (timer > 0) setDelayTimer(timer - 1)
  }

  def updateSoundTimer(): Unit = {
    val timer = getSoundTimer
    if (timer > 0) {
//      if (timer == 1) println("BEEP")
      setSoundTimer(timer - 1)
    }
  }

  // Accessors and mutators for emulator internal state..
  // These handle some bit twiddling since jvm does not have native unsigned types.
  def getRegisterValue(x: Int): Int = registers(x) & 0xff

  def setRegisterValue(x: Int, value: Int): Unit = registers(x) = value.toByte

  def getAddressRegisterValue: Int = addressRegister & 0xfff

  def setAddressRegisterValue(value: Int): Unit = addressRegister = value & 0xfff

  def getMemoryAt(index: Int): Int = memory(index) & 0xff

  def setMemoryAt(index: Int, value: Int): Unit = memory(index) = value.toByte

  def getProgramCounter: Int = programCounter & 0xfff

  def incrementProgramCounter(): Unit = programCounter = (programCounter + 2) & 0xfff

  def setProgramCounter(value: Int): Unit = programCounter = value & 0xfff

  def getDelayTimer: Int = delayTimer & 0xff

  def setDelayTimer(value: Int): Unit = delayTimer = value & 0xff

  def getSoundTimer: Int = soundTimer & 0xff

  def setSoundTimer(value: Int): Unit = soundTimer = value & 0xff

  def pushStack(value: Int): Unit = {
    if (stackPointer == stack.length) throw new StackOverflowException("Stack overflow")
    stack(stackPointer) = value
    stackPointer += 1
  }

  def popStack(): Int = {
    if (stackPointer == 0) throw new StackUnderflowException("Stack underflow")
    stackPointer -= 1
    stack(stackPointer)
  }

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
}
