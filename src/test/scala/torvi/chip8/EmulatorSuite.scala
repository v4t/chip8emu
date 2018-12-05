package torvi.chip8

import org.scalatest.FunSuite;
import org.scalatest.Matchers._

class EmulatorSuite extends FunSuite {

  test("Newly initialized emulator should have correct state") {
    val emu = new Emulator();
    assert(emu.programCounter == 0x200)
    assert(emu.stackPointer == 0)
    assert(emu.soundTimer == 0)
    assert(emu.delayTimer == 0)

    assert(emu.memory.length == 4096)
    exactly (4096, emu.memory) should be (0)

    assert(emu.registers.length == 16)
    exactly (16, emu.registers) should be (0)

    assert(emu.addressRegister == 0)

    assert(emu.stack.isEmpty)

    assert(emu.keyboardInput.length == 16)
    exactly (16, emu.keyboardInput) should be (false)
  }

  test("ROM is loaded into correct position in memory") {
    val rom = Array[Byte](4, 49, 122, -1)
    val emu = new Emulator();
    emu.loadRom(rom);
    assert(emu.memory(512) == 4)
    assert(emu.memory(513) == 49)
    assert(emu.memory(514) == 122)
    assert(emu.memory(515) == -1)
    exactly (4092, emu.memory) should be (0)
  }

  test("Loading too large ROM throws exception") {
    val rom = Array.fill[Byte](4096)(12)
    val emu = new Emulator()
    assertThrows[RomTooLargeException]{
      emu.loadRom(rom)
    }
  }

}

