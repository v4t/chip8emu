package torvi.chip8

import org.scalatest.FunSuite

class InstructionSuite extends  FunSuite {

  test("00E0: Clear screen instruction") {
    val emu = new Emulator()
    emu.programCounter = 0x259
    emu.drawFlag = false

    Instruction.clearScreen(emu, 0x00e0)

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

  test("00EE: Return from subroutine throws error if stack underflow") {
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
}
