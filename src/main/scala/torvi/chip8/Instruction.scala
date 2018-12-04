package torvi.chip8

object Instruction {
  /**
    * 00E0 - Clears the screen.
    */
  def clearScreen(emulator: Emulator, opCode: Short): Unit = {
    emulator.drawFlag = true
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 00EE - Returns from a subroutine.
    */
  def returnFromSub(emulator: Emulator, opCode: Short): Unit = {
    if (emulator.stack.isEmpty) throw new StackUnderflowException("Stack underflow")
    emulator.programCounter = (emulator.stack.pop() + 2).toShort
  }

  /**
    * 1NNN - Jump to address at NNN.
    */
  def jumpToAddr(emulator: Emulator, opCode: Short): Unit = {
    emulator.programCounter = (opCode & 0x0fff).toShort
  }

  /**
    * 2NNN - Calls subroutine at NNN.
    */
  def callSub(emulator: Emulator, opCode: Short): Unit = {
    emulator.stack.push(emulator.programCounter)
    emulator.programCounter = (opCode & 0x0fff).toShort
  }

  /**
    * 3XNN - Skips the next instruction if VX equals NN.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxEqualsNn(emulator: Emulator, opCode: Short): Unit = {
    val vx = emulator.registers((opCode & 0x0f00) >> 8)
    val nn = opCode & 0x00ff
    if(vx == nn) emulator.programCounter = (emulator.programCounter + 2).toShort
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 4XNN - Skips the next instruction if VX doesn't equal NN.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotEqualsNn(emulator: Emulator, opCode: Short): Unit = {
    val vx = emulator.registers((opCode & 0x0f00) >> 8)
    val nn = opCode & 0x00ff
    if(vx != nn) emulator.programCounter = (emulator.programCounter + 2).toShort
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 5XY0 - Skips the next instruction if VX equals VY.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxEqualsVy(emulator: Emulator, opCode: Short): Unit = {
    val vx = emulator.registers((opCode & 0x0f00) >> 8)
    val vy = emulator.registers((opCode & 0x00f0) >> 4)
    if(vx == vy) emulator.programCounter = (emulator.programCounter + 2).toShort
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 6XNN - Sets VX to NN.
    */
  def setVxToNn(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val nn = opCode & 0x00ff
    emulator.registers(x) = nn.toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 7XNN - Adds NN to VX. (Carry flag is not changed)
    */
  def addNnToVx(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val nn = opCode & 0x00ff
    emulator.registers(x) = (emulator.registers(x) + nn).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY0 - Sets VX to the value of VY.
    */
  def setVxToVy(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val y = (opCode & 0x00f0) >> 4
    emulator.registers(x) = emulator.registers(y)
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY1 - Sets VX to VX or VY. (Bitwise OR operation)
    */
  def setVxToVxOrVy(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.registers(x)
    val vy = emulator.registers((opCode & 0x00f0) >> 4)
    emulator.registers(x) = (vx | vy).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY2 - Sets VX to VX and VY. (Bitwise AND operation)
    */
  def setVxToVxAndVy(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.registers(x)
    val vy = emulator.registers((opCode & 0x00f0) >> 4)
    emulator.registers(x) = (vx & vy).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY3 - Sets VX to VX xor VY
    */
  def setVxToVxXorVy(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.registers(x)
    val vy = emulator.registers((opCode & 0x00f0) >> 4)
    emulator.registers(x) = (vx ^ vy).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY4 - Adds VY to VX. VF is set to 1 when there's a carry,
    * and to 0 when there isn't.
    */
  def addVyToVx(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val intVx = emulator.registers(x) & 0xff
    val intVy = emulator.registers((opCode & 0x00f0) >> 4) & 0xff
    if(intVy > (0xff - intVx)) emulator.registers(15) = 1
    else emulator.registers(15) = 0

    emulator.registers(x) = (intVx + intVy).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY5 - VY is subtracted from VX. VF is set to 0 when there's a borrow,
    * and 1 when there isn't.
    */
  def subVyFromVx(emulator: Emulator, opCode: Short): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val intVx = emulator.registers(x) & 0xff
    val intVy = emulator.registers((opCode & 0x00f0) >> 4) & 0xff
    if(intVy > intVx) emulator.registers(15) = 0
    else emulator.registers(15) = 1

    emulator.registers(x) = (intVx - intVy).toByte
    emulator.programCounter = (emulator.programCounter + 2).toShort
  }

  /**
    * 8XY6 - Stores the least significant bit of VX in VF
    * and then shifts VX to the right by 1.
    */
  def shiftRightVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * 8XY7 - Sets VX to VY minus VX. VF is set to 0 when there's a borrow,
    * and 1 when there isn't.
    */
  def setVxToVyMinusVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * 8XYE - Stores the most significant bit of VX in VF
    * and then shifts VX to the left by 1.
    */
  def shiftLeftVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * 9XY0 - Skips the next instruction if VX doesn't equal VY.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotEqualsVy(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * ANNN - Sets I to the address NNN.
    */
  def setAddrRegToNn(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * BNNN - Jumps to the address NNN plus V0.
    */
  def jumpToAddrPlusV0(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * CXNN - ets VX to the result of a bitwise and operation on a random number
    * (Typically: 0 to 255) and NN.
    */
  def setVxToRand(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * DXYN - Draws a sprite at coordinate (VX, VY) that has a
    * width of 8 pixels and a height of N pixels.
    * Each row of 8 pixels is read as bit-coded starting from
    * memory location I; I value doesn’t change after the execution of
    * this instruction. As described above, VF is set to 1 if any
    * screen pixels are flipped from set to unset when the sprite
    * is drawn, and to 0 if that doesn’t happen
    */
  def drawVxVyN(emulator: Emulator, opCode: Short): Unit = {}


  /**
    * EX9E - Skips the next instruction if the key stored in VX is pressed.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxPressed(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * EXA1 - Skips the next instruction if the key stored in VX isn't pressed.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotPressed(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX07 - Sets VX to the value of the delay timer.
    */
  def setVxToDelayTimerValue(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX0A - A key press is awaited, and then stored in VX.
    * (Blocking Operation. All instruction halted until next key event)
    */
  def waitForKeyPress(emulator: Emulator, opCode: Short): Unit = {}
  /**
    * FX15 - Sets the delay timer to VX.
    */
  def setDelayTimerValueToVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX18 - Sets the delay timer to VX.
    */
  def setSoundTimerValueToVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX1E - Adds VX to I.
    */
  def addVxToAddrReg(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX29 - Sets I to the location of the sprite for the character in VX.
    * Characters 0-F (in hexadecimal) are represented by a 4x5 font.
    */
  def setAddrRegToSpriteAddr(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX33 - Stores the binary-coded decimal representation of VX,
    * with the most significant of three digits at the address in I,
    * the middle digit at I plus 1, and the least significant digit
    * at I plus 2. (In other words, take the decimal representation of VX,
    * place the hundreds digit in memory at location in I, the tens digit
    * at location I+1, and the ones digit at location I+2.)
    */
  def storeBcdForVx(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX55 - Stores V0 to VX (including VX) in memory starting at address I.
    * The offset from I is increased by 1 for each value written, but I itself
    * is left unmodified.
    */
  def regDump(emulator: Emulator, opCode: Short): Unit = {}

  /**
    * FX65 - Fills V0 to VX (including VX) with values from memory starting at
    * address I. The offset from I is increased by 1 for each value written,
    * but I itself is left unmodified.
    */
  def regLoad(emulator: Emulator, opCode: Short): Unit = {}

}

