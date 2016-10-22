package gnes

type Emulator struct {
	cpu *CPU
	ppu *PPU
	mem *Memory
}

func NewEmulator(rom *ROM) *Emulator {
	emu := Emulator{}
	emu.ppu = NewPPU(emu.invokeNMI)
	emu.mem = &Memory{rom: rom, ppu: emu.ppu}
	emu.cpu = NewCPU(emu.mem)

	return &emu
}

func (e *Emulator) Step() {
	for {
		cpuCycles := e.cpu.step()
		for i := uint64(0); i < 3*cpuCycles; i++ {
			e.ppu.step()
		}
	}
}

func (e *Emulator) invokeNMI() {
	e.cpu.invokeNMI()
}
