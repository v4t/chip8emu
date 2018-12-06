package torvi.chip8

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class InstructionSuite extends  FunSuite {

  test("00E0: Clear screen instruction") {
    val emu = new Emulator()
    emu.programCounter = 0x259
    emu.drawFlag = false
    emu.screenPixels(0) = true
    emu.screenPixels(12) = true

    Instruction.clearScreen(emu, 0x00e0)
    exactly(emu.screenWidth * emu.screenHeight, emu.screenPixels) should be(false)
    assert(emu.programCounter == (0x259 + 2).toShort)
    assert(emu.drawFlag)
  }

  test("00EE: Return from subroutine") {
    val emu = new Emulator()
    val addr = 0x305.toByte
    emu.programCounter = 0x300
    emu.stack.push(addr)

    Instruction.returnFromSub(emu, 0x00ee.toShort)

    assert(emu.programCounter == (addr + 2).toShort)
    assert(emu.stack.isEmpty)
  }

  test("00EE: Return from subroutine throws error if stack is empty") {
    val emu = new Emulator()

    assertThrows[StackUnderflowException]{
      Instruction.returnFromSub(emu, 0x00ee.toShort)
    }
  }

  test("1NNN: Jump") {
    val emu = new Emulator()
    emu.programCounter = 0x300

    Instruction.jumpToAddr(emu, 0x1234)
    assert(emu.programCounter == 0x234.toShort)
  }

  test("2NNN: Call subroutine") {
    val emu = new Emulator()
    emu.programCounter = 0x300

    Instruction.callSub(emu, 0x2234)
    assert(emu.stack.length == 1)
    assert(emu.stack.pop() == 0x300)
    assert(emu.programCounter == 0x234.toShort)
  }

  test("2NNN: Call subroutine throws error if stack overflows") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    (0 to 15).foreach(_ => Instruction.callSub(emu, 0x2234))

    assertThrows[StackOverflowException]{
      Instruction.callSub(emu, 0x2234)
    }
  }

  test("3XNN: Skips next instruction if VX equals NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x7

    Instruction.skipVxEqualsNn(emu, 0x3407)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("3XNN: Doesn't skip next instruction if VX does not equal NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5

    Instruction.skipVxEqualsNn(emu, 0x3404)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("4XNN: Skips next instruction if VX does not equal NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5

    Instruction.skipVxNotEqualsNn(emu, 0x4404)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("4XNN: Doesn't skip next instruction if VX equals NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x7

    Instruction.skipVxNotEqualsNn(emu, 0x4407)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("5XY0: Skips next instruction if VX equals VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x7
    emu.registers(5) = 0x7

    Instruction.skipVxEqualsVy(emu, 0x5450)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("5XY0: Doesn't skip next instruction if VX does not equal VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x6

    Instruction.skipVxEqualsNn(emu, 0x5450)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("6XNN: Set VX to NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5

    Instruction.setVxToNn(emu, 0x6466)
    assert(emu.registers(4) == 0x66)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("7XNN: Add NN to VX") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5

    Instruction.addNnToVx(emu, 0x7466)
    assert(emu.registers(4) == 0x5 + 0x66)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY0: Set VX to VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.setVxToVy(emu, 0x8450.toShort)
    assert(emu.registers(4) == emu.registers(5))
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY1: Set VX to VX | VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.setVxToVxOrVy(emu, 0x8451.toShort)
    assert(emu.registers(4) == (0x5 | 0x68).toByte)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY2: Set VX to VX & VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.setVxToVxAndVy(emu, 0x8452.toShort)
    assert(emu.registers(4) == (0x5 & 0x68).toByte)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY3: Set VX to VX ^ VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.setVxToVxXorVy(emu, 0x8453.toShort)
    assert(emu.registers(4) == (0x5 ^ 0x68).toByte)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY4: Add VY to VX (sets VF to 1 when there's a carry)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0xaa.toByte
    emu.registers(5) = 0xbb.toByte

    Instruction.addVyToVx(emu, 0x8454.toShort)
    assert(emu.registers(4) == (0xaa + 0xbb).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY4: Add VY to VX (sets VF to 0 when there's no carry)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.addVyToVx(emu, 0x8454.toShort)
    assert(emu.registers(4) == (0x5 + 0x68).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY5: Subtract VY from VX (sets VF to 1 when there's no borrow)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0xaa.toByte
    emu.registers(5) = 0x99.toByte

    Instruction.subVyFromVx(emu, 0x8455.toShort)
    assert(emu.registers(4) == (0xaa - 0x99).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY5: Subtract VY from VX(sets VF to 0 when there's a borrow)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.addressRegister = 0x980
    emu.registers(4) = 0x5
    emu.registers(5) = 0x68

    Instruction.subVyFromVx(emu, 0x8455.toShort)
    assert(emu.registers(4) == (0x5 - 0x68).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.addressRegister == 0x980)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY6: Store least significant bit (1) to VF and right shift VX by one") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5.toByte

    Instruction.shiftRightVx(emu, 0x8456.toShort)
    assert(emu.registers(4) == (0x5 >> 1).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY6: Store least significant bit (0) to VF and right shift VX by one") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x4.toByte

    Instruction.shiftRightVx(emu, 0x8456.toShort)
    assert(emu.registers(4) == (0x4 >> 1).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY7: Set VX to VY - VX and set VF to 0 when there is a borrow") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5.toByte
    emu.registers(5) = 0x3.toByte

    Instruction.setVxToVyMinusVx(emu, 0x8457.toShort)
    assert(emu.registers(4) == (0x3 - 0x5).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XY7: Set VX to VY - VX and set VF to 1 when there is no borrow") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x4.toByte
    emu.registers(5) = 0x7.toByte

    Instruction.setVxToVyMinusVx(emu, 0x8457.toShort)
    assert(emu.registers(4) == (0x7 - 0x4).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XYE: Store most significant bit (1) to VF and left shift VX by one") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0xaa.toByte

    Instruction.shiftLeftVx(emu, 0x845e.toShort)
    assert(emu.registers(4) == (0xaa << 1).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("8XYE: Store most significant bit (0) to VF and left shift VX by one") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x4.toByte

    Instruction.shiftLeftVx(emu, 0x845e.toShort)
    assert(emu.registers(4) == (0x4 << 1).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("9XY0: Skips next instruction if VX does not equal VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.registers(5) = 0x6

    Instruction.skipVxNotEqualsVy(emu, 0x9450.toShort)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("9XY0: Doesn't skip next instruction if VX equals VY") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x7
    emu.registers(5) = 0x7

    Instruction.skipVxNotEqualsVy(emu, 0x9450.toShort)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("ANNN: set address register to NNN") {
    val emu = new Emulator()
    emu.programCounter = 0x300

    Instruction.setAddrRegToNn(emu, 0xabcd.toShort)
    assert(emu.addressRegister == 0xbcd)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("BNNN: Jump to address NNN + V0") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(0) = 0x5

    Instruction.jumpToAddrPlusV0(emu, 0xb123.toShort)
    assert(emu.programCounter == (0x123 + 0x5).toShort)
  }

  test("CXNN: Set VX to random number & NN") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5

    Instruction.setVxToRand(emu, 0xc400.toShort)
    assert(emu.registers(4) != 0x5)
    assert(emu.programCounter == (0x300 + 2).toShort)

    val cur = emu.registers(4)
    Instruction.setVxToRand(emu, 0xc4ff.toShort)
    assert(emu.registers(4) != cur)
    assert(emu.programCounter == (0x300 + 4).toShort) // this check might fail
  }

  test("DXYN: Draw sprite at coordinate VX, VY. VF is left unset because no pixels are flipped") {
    val emu = new Emulator()
    emu.programCounter = 0x600
    emu.drawFlag = false
    emu.registers(4) = 0x0
    emu.registers(5) = 0x0
    emu.addressRegister = 0x800

    emu.memory(0x800) = 48 // 00110000
    emu.memory(0x801) = 1  // 00000001

    Instruction.drawVxVyN(emu, 0xd452.toShort)

    // first row of sprite should be 00110000
    assert(!emu.screenPixels(0))
    assert(!emu.screenPixels(1))
    assert(emu.screenPixels(2))
    assert(emu.screenPixels(3))
    assert(!emu.screenPixels(4))
    assert(!emu.screenPixels(5))
    assert(!emu.screenPixels(6))
    assert(!emu.screenPixels(7))

    // second row of sprite 00000001
    assert(!emu.screenPixels(0 + 64))
    assert(!emu.screenPixels(1 + 64))
    assert(!emu.screenPixels(2 + 64))
    assert(!emu.screenPixels(3 + 64))
    assert(!emu.screenPixels(4 + 64))
    assert(!emu.screenPixels(5 + 64))
    assert(!emu.screenPixels(6 + 64))
    assert(emu.screenPixels(7 + 64))

    // rest of emulator state
    assert(emu.programCounter == (0x600 + 2).toShort)
    assert(emu.registers(15) == 0)
    assert(emu.drawFlag)
  }

  test("DXYN: Draw sprite at coordinate VX, VY. VF is set to 1 because pixel was flipped") {
    val emu = new Emulator()
    emu.programCounter = 0x600
    emu.drawFlag = false
    emu.registers(4) = 0x0
    emu.registers(5) = 0x0
    emu.addressRegister = 0x800

    emu.screenPixels(2) = true
    emu.memory(0x800) = 48 // 00110000
    emu.memory(0x801) = 1  // 00000001

    Instruction.drawVxVyN(emu, 0xd452.toShort)

    // first row of sprite should be 00010000
    assert(!emu.screenPixels(0))
    assert(!emu.screenPixels(1))
    assert(!emu.screenPixels(2)) // flipped pixel
    assert(emu.screenPixels(3))
    assert(!emu.screenPixels(4))
    assert(!emu.screenPixels(5))
    assert(!emu.screenPixels(6))
    assert(!emu.screenPixels(7))

    // second row of sprite 00000001
    assert(!emu.screenPixels(0 + 64))
    assert(!emu.screenPixels(1 + 64))
    assert(!emu.screenPixels(2 + 64))
    assert(!emu.screenPixels(3 + 64))
    assert(!emu.screenPixels(4 + 64))
    assert(!emu.screenPixels(5 + 64))
    assert(!emu.screenPixels(6 + 64))
    assert(emu.screenPixels(7 + 64))

    // rest of emulator state
    assert(emu.programCounter == (0x600 + 2).toShort)
    assert(emu.registers(15) == 1)
    assert(emu.drawFlag)
  }


  test("EX9E: Skip next instruction if key stored in VX is pressed") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.keyboardInput(5) = true

    Instruction.skipVxPressed(emu, 0xe49e.toShort)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("EX9E: Don't skip next instruction if key stored in VX is not pressed") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.keyboardInput(5) = false

    Instruction.skipVxPressed(emu, 0xe49e.toShort)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("EXA1: Skip next instruction if key stored in VX is not pressed") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.keyboardInput(5) = false

    Instruction.skipVxNotPressed(emu, 0xe4a1.toShort)
    assert(emu.programCounter == (0x300 + 4).toShort)
  }

  test("EXA1: Don't skip next instruction if key stored in VX is pressed") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.keyboardInput(5) = true

    Instruction.skipVxNotPressed(emu, 0xe4a1.toShort)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX07: Set VX to value of delay timer") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.delayTimer = 0x11

    Instruction.setVxToDelayTimerValue(emu, 0xf407.toShort)
    assert(emu.registers(4) == 0x11)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX0A: Await for key press and store key to VX") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0
    emu.delayTimer = 0x99.toByte

    Instruction.waitForKeyPress(emu, 0xf40a.toShort)
    assert(emu.registers(4) == 0)
    assert(emu.programCounter == 0x300.toShort)

    emu.keyboardInput(5) = true
    Instruction.waitForKeyPress(emu, 0xf40a.toShort)
    assert(emu.registers(4) == 5)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX15: Set delay timer to VX") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.delayTimer = 0x11

    Instruction.setDelayTimerValueToVx(emu, 0xf415.toShort)
    assert(emu.delayTimer == 0x5)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX18: Set sound timer to VX") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.soundTimer = 0x11

    Instruction.setSoundTimerValueToVx(emu, 0xf418.toShort)
    assert(emu.soundTimer == 0x5)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX1E: Adds VX to address register (sets VF to 1 when there's a carry)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0xaa.toByte
    emu.addressRegister = 0xbb.toByte

    Instruction.addVxToAddrReg(emu, 0xf41e.toShort)
    assert(emu.addressRegister == (0xaa + 0xbb).toByte)
    assert(emu.registers(15) == 1)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX1E: Adds VX to address register (sets VF to 0 when there's no carry)") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 0x5
    emu.addressRegister = 0x6.toByte

    Instruction.addVxToAddrReg(emu, 0xf41e.toShort)
    assert(emu.addressRegister == (0x5 + 0x6).toByte)
    assert(emu.registers(15) == 0)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX33: Store binary-coded decimal representation of VX to I, I+1 and I+2") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(4) = 172.toByte
    emu.addressRegister = 0x987

    Instruction.storeBcdForVx(emu, 0xf433.toShort)
    assert(emu.memory(0x987) == 1)
    assert(emu.memory(0x988) == 7)
    assert(emu.memory(0x989) == 2)
    assert(emu.addressRegister == 0x987)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX55: Store V0 to VX in memory starting at address register value") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.registers(0) = 0x25.toByte
    emu.registers(2) = 0xff.toByte
    emu.registers(3) = 0x12.toByte
    emu.addressRegister = 0x983

    Instruction.regDump(emu, 0xf355.toShort)
    assert(emu.memory(0x983) == 0x25.toByte)
    assert(emu.memory(0x984) == 0.toByte)
    assert(emu.memory(0x985) == 0xff.toByte)
    assert(emu.memory(0x986) == 0x12.toByte)
    assert(emu.addressRegister == 0x983)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }

  test("FX65: Fill V0 to VX with values from memory starting at address register") {
    val emu = new Emulator()
    emu.programCounter = 0x300
    emu.memory(0x983) = 0x25.toByte
    emu.memory(0x985) = 0xff.toByte
    emu.memory(0x986) = 0x12.toByte
    emu.addressRegister = 0x983

    Instruction.regLoad(emu, 0xf355.toShort)
    assert(emu.registers(0) == 0x25.toByte)
    assert(emu.registers(1) == 0.toByte)
    assert(emu.registers(2) == 0xff.toByte)
    assert(emu.registers(3) == 0x12.toByte)
    assert(emu.addressRegister == 0x983)
    assert(emu.programCounter == (0x300 + 2).toShort)
  }
}
