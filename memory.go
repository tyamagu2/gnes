package gnes

type Memory struct {
	ROM  *ROM
	Data [0x10000]uint8
}

func (m *Memory) readBytes(addr uint16, _n uint8) []uint8 {
	n := uint16(_n)
	if m.ROM.NumRPG == 1 && addr >= 0xc000 {
		b := addr - 0xc000
		return m.ROM.RPG[b : b+n]
	} else if addr >= 0x8000 {
		b := addr - 0x8000
		return m.ROM.RPG[b : b+n]
	}
	return []uint8{}
}

func (m *Memory) Read(addr uint16) uint8 {
	if m.ROM.NumRPG == 1 && addr >= 0xc000 {
		return m.ROM.RPG[addr-0xc000]
	} else if addr >= 0x8000 {
		return m.ROM.RPG[addr-0x8000]
	} else {
		return m.Data[addr]
	}
}

func (m *Memory) Write(addr uint16, data uint8) {
	m.Data[addr] = data
	if addr < 0x2000 {
		m.Data[0x0800+addr] = data
		m.Data[0x1000+addr] = data
		m.Data[0x1800+addr] = data
	}
}
