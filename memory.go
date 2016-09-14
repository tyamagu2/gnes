package gnes

import "log"

type Memory struct {
	ROM  *ROM
	Data [0x10000]byte
}

func (m *Memory) Read(addr uint16) byte {
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
