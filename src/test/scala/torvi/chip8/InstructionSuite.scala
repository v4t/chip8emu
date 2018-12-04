package torvi.chip8

import org.scalatest.FunSuite;

class InstructionSuite extends  FunSuite {

  test("00E0: Clear screen instruction") {
    val emu = new Emulator();
    emu.programCounter = 0x259;
    emu.drawFlag = false;
    Instruction.clearScreen(emu, 0x00e0)

    assert(emu.programCounter == (0x259 + 2).toShort)
    assert(emu.drawFlag)
  }
}
