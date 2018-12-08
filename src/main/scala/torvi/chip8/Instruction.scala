package torvi.chip8

object Instruction {

  val rng = new scala.util.Random(System.currentTimeMillis())

  /**
    * 00E0 - Clears the screen.
    */
  def clearScreen(emulator: Emulator, opCode: Int): Unit = {
    val screenSize = emulator.screenPixels.length - 1
    (0 to screenSize).foreach(i => emulator.screenPixels(i) = false)

    emulator.incrementProgramCounter()
  }

  /**
    * 00EE - Returns from a subroutine.
    */
  def returnFromSub(emulator: Emulator, opCode: Int): Unit = {
    if (emulator.stack.isEmpty) throw new StackUnderflowException("Stack underflow")
    emulator.setProgramCounter(emulator.stack.pop() + 2)
  }

  /**
    * 1NNN - Jump to address at NNN.
    */
  def jumpToAddr(emulator: Emulator, opCode: Int): Unit = {
    emulator.setProgramCounter(opCode & 0x0fff)
  }

  /**
    * 2NNN - Calls subroutine at NNN.
    */
  def callSub(emulator: Emulator, opCode: Int): Unit = {
    if (emulator.stack.length == 16) throw new StackOverflowException("Stack overflow")
    emulator.stack.push(emulator.getProgramCounter())
    emulator.setProgramCounter(opCode & 0x0fff)
  }

  /**
    * 3XNN - Skips the next instruction if VX equals NN.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxEqualsNn(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val nn = opCode & 0x00ff

    if (vx == nn) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * 4XNN - Skips the next instruction if VX doesn't equal NN.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotEqualsNn(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val nn = opCode & 0x00ff

    if (vx != nn) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * 5XY0 - Skips the next instruction if VX equals VY.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxEqualsVy(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    if (vx == vy) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * 6XNN - Sets VX to NN.
    */
  def setVxToNn(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val nn = opCode & 0x00ff
    emulator.setRegisterValue(x, nn)
    emulator.incrementProgramCounter()
  }

  /**
    * 7XNN - Adds NN to VX. (Carry flag is not changed)
    */
  def addNnToVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val nn = opCode & 0x00ff

    emulator.setRegisterValue(x, vx + nn)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY0 - Sets VX to the value of VY.
    */
  def setVxToVy(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)
    emulator.setRegisterValue(x, vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY1 - Sets VX to VX or VY. (Bitwise OR operation)
    */
  def setVxToVxOrVy(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)
    emulator.setRegisterValue(x, vx | vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY2 - Sets VX to VX and VY. (Bitwise AND operation)
    */
  def setVxToVxAndVy(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    emulator.setRegisterValue(x, vx & vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY3 - Sets VX to VX xor VY
    */
  def setVxToVxXorVy(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    emulator.setRegisterValue(x, vx ^ vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY4 - Adds VY to VX. VF is set to 1 when there's a carry,
    * and to 0 when there isn't.
    */
  def addVyToVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    if (vy > (0xff - vx)) emulator.setRegisterValue(0xf, 1)
    else emulator.setRegisterValue(0xf, 0)

    emulator.setRegisterValue(x, vx + vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY5 - VY is subtracted from VX. VF is set to 0 when there's a borrow,
    * and 1 when there isn't.
    */
  def subVyFromVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    if (vy > vx) emulator.setRegisterValue(0xf, 0)
    else emulator.setRegisterValue(0xf, 1)

    emulator.setRegisterValue(x, vx - vy)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY6 - Stores the least significant bit of VX in VF
    * and then shifts VX to the right by 1.
    */
  def shiftRightVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)

    emulator.setRegisterValue(0xf, vx & 0x1)
    emulator.setRegisterValue(x, vx >> 1)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XY7 - Sets VX to VY minus VX. VF is set to 0 when there's a borrow,
    * and 1 when there isn't.
    */
  def setVxToVyMinusVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    if (vx > vy) emulator.setRegisterValue(0xf, 0)
    else emulator.setRegisterValue(0xf, 1)

    emulator.setRegisterValue(x, vy - vx)
    emulator.incrementProgramCounter()
  }

  /**
    * 8XYE - Stores the most significant bit of VX in VF
    * and then shifts VX to the left by 1.
    */
  def shiftLeftVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val vx = emulator.getRegisterValue(x)

    emulator.setRegisterValue(0xf, (vx >> 7) & 0x1)
    emulator.setRegisterValue(x, vx << 1)
    emulator.incrementProgramCounter()
  }

  /**
    * 9XY0 - Skips the next instruction if VX doesn't equal VY.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotEqualsVy(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)

    if (vx != vy) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * ANNN - Sets I to the address NNN.
    */
  def setAddrRegToNn(emulator: Emulator, opCode: Int): Unit = {
    emulator.setAddressRegisterValue(opCode & 0x0fff)
    emulator.incrementProgramCounter()
  }

  /**
    * BNNN - Jumps to the address NNN plus V0.
    */
  def jumpToAddrPlusV0(emulator: Emulator, opCode: Int): Unit = {
    emulator.setProgramCounter((opCode & 0x0fff) + emulator.getRegisterValue(0))
  }

  /**
    * CXNN - ets VX to the result of a bitwise and operation on a random number
    * (Typically: 0 to 255) and NN.
    */
  def setVxToRand(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val nn = opCode & 0x00ff
    val randomValue = 0 + rng.nextInt((255 - 0) + 1)

    emulator.setRegisterValue(x, randomValue & nn)
    emulator.incrementProgramCounter()
  }

  /**
    * DXYN - Draws a sprite at coordinate (VX, VY) that has a
    * width of 8 pixels and a height of N pixels.
    * Each row of 8 pixels is read as bit-coded starting from
    * memory location I; I value doesn’t change after the execution of
    * this instruction. As described above, VF is set to 1 if any
    * screen pixels are flipped from set to unset when the sprite
    * is drawn, and to 0 if that doesn’t happen
    */
  def drawVxVyN(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val vy = emulator.getRegisterValue((opCode & 0x00f0) >> 4)
    val n = opCode & 0x000f
    emulator.setRegisterValue(0xf, 0)

    for (yLine <- 0 until n) {
      val pixel = emulator.getMemoryAt(emulator.getAddressRegisterValue() + yLine)
      for (xLine <- 0 until 8) {
        if ((pixel & (0x80 >> xLine)) != 0) {
          val pixelPos = vx + xLine + ((vy + yLine) * emulator.screenWidth)
          if(pixelPos >= 0 && pixelPos < emulator.screenPixels.length){
            if(emulator.screenPixels(pixelPos)) emulator.setRegisterValue(0xf, 1)
            emulator.screenPixels(pixelPos) = !emulator.screenPixels(pixelPos)
          }
        }
      }
    }
    emulator.incrementProgramCounter()
  }

  /**
    * EX9E - Skips the next instruction if the key stored in VX is pressed.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxPressed(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    if (vx < 16 && emulator.keyboardInput(vx)) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * EXA1 - Skips the next instruction if the key stored in VX isn't pressed.
    * (Usually the next instruction is a jump to skip a code block)
    */
  def skipVxNotPressed(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    if (vx < 16 && !emulator.keyboardInput(vx)) emulator.incrementProgramCounter()
    emulator.incrementProgramCounter()
  }

  /**
    * FX07 - Sets VX to the value of the delay timer.
    */
  def setVxToDelayTimerValue(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    emulator.setRegisterValue(x, emulator.getDelayTimer())
    emulator.incrementProgramCounter()
  }

  /**
    * FX0A - A key press is awaited, and then stored in VX.
    * (Blocking Operation. All instruction halted until next key event)
    */
  def waitForKeyPress(emulator: Emulator, opCode: Int): Unit = {
    val keyPressed = emulator.keyboardInput.indexWhere(_ == true)
    if (keyPressed == -1) return

    val x = (opCode & 0x0f00) >> 8
    emulator.setRegisterValue(x, keyPressed)
    emulator.incrementProgramCounter()
  }

  /**
    * FX0xf - Sets the delay timer to VX.
    */
  def setDelayTimerValueToVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    emulator.setDelayTimer(emulator.getRegisterValue(x))
    emulator.incrementProgramCounter()
  }

  /**
    * FX18 - Sets the sound timer to VX.
    */
  def setSoundTimerValueToVx(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    emulator.setSoundTimer(emulator.getRegisterValue(x))
    emulator.incrementProgramCounter()
  }

  /**
    * FX1E - Adds VX to I.
    */
  def addVxToAddrReg(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)

    if (emulator.getAddressRegisterValue() + vx > 0xfff) emulator.setRegisterValue(0xf, 1)
    else emulator.setRegisterValue(0xf, 0)

    emulator.setAddressRegisterValue(vx + emulator.getAddressRegisterValue())
    emulator.incrementProgramCounter()
  }

  /**
    * FX29 - Sets I to the location of the sprite for the character in VX.
    * Characters 0-F (in hexadecimal) are represented by a 4x5 font.
    */
  def setAddrRegToSpriteAddr(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)

    emulator.setAddressRegisterValue(emulator.spriteStartAddr + (vx * emulator.spriteLength))
    emulator.incrementProgramCounter()
  }

  /**
    * FX33 - Stores the binary-coded decimal representation of VX,
    * with the most significant of three digits at the address in I,
    * the middle digit at I plus 1, and the least significant digit
    * at I plus 2. (In other words, take the decimal representation of VX,
    * place the hundreds digit in memory at location in I, the tens digit
    * at location I+1, and the ones digit at location I+2.)
    */
  def storeBcdForVx(emulator: Emulator, opCode: Int): Unit = {
    val vx = emulator.getRegisterValue((opCode & 0x0f00) >> 8)
    val addrReg = emulator.getAddressRegisterValue()

    emulator.setMemoryAt(addrReg, vx / 100)
    emulator.setMemoryAt(addrReg + 1, (vx / 10) % 10)
    emulator.setMemoryAt(addrReg + 2, (vx % 100) % 10)
    emulator.incrementProgramCounter()
  }

  /**
    * FX55 - Stores V0 to VX (including VX) in memory starting at address I.
    * The offset from I is increased by 1 for each value written, but I itself
    * is left unmodified.
    */
  def regDump(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8
    val addrReg = emulator.getAddressRegisterValue()
    (0 to x).foreach(i =>
      emulator.setMemoryAt(addrReg + i, emulator.getRegisterValue(i))
    )
    emulator.incrementProgramCounter()
  }

  /**
    * FX65 - Fills V0 to VX (including VX) with values from memory starting at
    * address I. The offset from I is increased by 1 for each value written,
    * but I itself is left unmodified.
    */
  def regLoad(emulator: Emulator, opCode: Int): Unit = {
    val x = (opCode & 0x0f00) >> 8 & 0xf

    (0 to x).foreach { i =>
      val memAddr = emulator.getAddressRegisterValue() + i
      emulator.setRegisterValue(i, emulator.getMemoryAt(memAddr))
    }
    emulator.incrementProgramCounter()
  }

}
