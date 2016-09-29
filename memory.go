package gnes

type Memory struct {
	rom *ROM
	ram [0x8000]uint8
}

func (m *Memory) readBytes(addr uint16, _n uint8) []uint8 {
	n := uint16(_n)
	if m.rom.NumRPG == 1 && addr >= 0xc000 {
		b := addr - 0xc000
		return m.rom.RPG[b : b+n]
	} else if addr >= 0x8000 {
		b := addr - 0x8000
		return m.rom.RPG[b : b+n]
	} else {
		a := addr % 0x0800
		return m.ram[a : a+n]
	}
}

func (m *Memory) read(addr uint16) uint8 {
	if m.rom.NumRPG == 1 && addr >= 0xc000 {
		return m.rom.RPG[addr-0xc000]
	} else if addr >= 0x8000 {
		return m.rom.RPG[addr-0x8000]
	} else if addr <= 0x2000 {
		return m.ram[addr%0x0800]
	}

	return m.ram[addr] // For now
}

func (m *Memory) write(addr uint16, data uint8) {
	m.ram[addr] = data
}
