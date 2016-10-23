package gnes

const (
	ADDR_MIRROR_LOW  = 0x4000
	ADDR_PALETTE_LOW = 0x3F00
)

// VRAM
type VRAM struct {
	data    [0x4000]uint8
	palette [0x20]uint8
}

func (v *VRAM) read(_addr uint16) uint8 {
	addr := _addr % ADDR_MIRROR_LOW

	if addr >= ADDR_PALETTE_LOW {
		return v.readPalette(addr)
	}

	return v.data[addr]
}

func (v *VRAM) paletteAddr(_addr uint16) uint16 {
	addr := _addr % 0x20

	// http://wiki.nesdev.com/w/index.php/PPU_palettes
	// Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C.
	if addr > 0x10 && addr%4 == 0 {
		return addr - 0x10
	}

	return addr
}

func (v *VRAM) readPalette(addr uint16) uint8 {
	return v.palette[v.paletteAddr(addr)]
}

func (v *VRAM) write(_addr uint16, d uint8) {
	addr := _addr % ADDR_MIRROR_LOW
	if addr >= ADDR_PALETTE_LOW {
		v.writePalette(addr, d)
	} else {
		v.data[addr] = d
	}
}

func (v *VRAM) writePalette(_addr uint16, d uint8) {
	addr := v.paletteAddr(_addr)
	v.palette[addr] = d
	if addr%4 == 2 {
		v.palette[addr] &= 0xE3
	}
}
