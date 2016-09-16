package gnes

import "log"

type Memory struct {
	ROM  *ROM
	Data [0x10000]byte
}

func (m *Memory) readBytes(addr uint16, _n uint8) []byte {
	n := uint16(_n)
	if m.ROM.NumRPG == 1 && addr >= 0xc000 {
		b := addr - 0xc000
		return m.ROM.RPG[b : b+n]
	} else if addr >= 0x8000 {
		b := addr - 0x8000
		return m.ROM.RPG[b : b+n]
	}
	return []byte{}
}

func (m *Memory) Read(addr uint16) uint8 {
	if m.ROM.NumRPG == 1 && addr >= 0xc000 {
		return m.ROM.RPG[addr-0xc000]
	} else if addr >= 0x8000 {
		return m.ROM.RPG[addr-0x8000]
	}

	return 0
}

func (m *Memory) Write(addr uint16, data uint8) {
	if addr < 0x8000 {
		m.Data[addr] = data
	} else {
		log.Fatalf("Memory.Write Unsupported address 0x%x", addr)
	}
}
