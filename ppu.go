package gnes

import (
	"fmt"
	"log"
)

const (
	cyclesPerSL   = 341
	slPerFrame    = 262
	vBlankSL      = 241
	preRenderLine = 261
)

const (
	flagSpriteOverflow = 1 << (5 + iota)
	flagSprite0Hit
	flagVBlank
)

// Picture Processing Unit
type PPU struct {
	cycles int
	sl     int // Scan Line
	vram   *VRAM

	buffered uint8

	// Registers
	v uint16 // Current VRAM address
	t uint16 // Temporary VRAM address
	x uint8  // Fine X scroll
	w uint8  // First or second write toggle

	// PPUCTRL
	baseNametableAddr     uint8
	vramAddrInc32         bool
	spritPatternTableAddr bool

	// PPUSTATUS
	spriteOverflow bool
	sprite0Hit     bool
	vblank         bool

	invokeNMI func()
}

func NewPPU(invokeNMI func()) *PPU {
	ppu := PPU{invokeNMI: invokeNMI, vram: &VRAM{}}
	ppu.reset()
	return &ppu
}

func (p *PPU) reset() {
	p.cycles = cyclesPerSL - 1
	p.sl = vBlankSL - 1
}

func (p *PPU) writeScroll(d uint8) {
	if p.w == 0 {
		// t: ....... ...HGFED = d: HGFED...
		// x:              CBA = d: .....CBA
		// w:                  = 1
		p.t = (p.t & 0xFFE0) | uint16(d)>>3
		p.x = d & 0x07
		p.w = 1
	} else {
		// t: CBA..HG FED..... = d: HGFEDCBA
		// w:                  = 0
		p.t = (p.t & 0x8C1F) | (uint16(d)&0x07)<<12 | (uint16(d)&0xF8)<<2
		p.w = 0
	}
}

func (p *PPU) writeAddr(d uint8) {
	if p.w == 0 {
		// t: .FEDCBA ........ = d: ..FEDCBA
		// t: X...... ........ = 0
		// w:                  = 1
		p.t = (p.t & 0x80FF) | (uint16(d&0x3F) << 8)
		p.w = 1
	} else {
		// t: ....... HGFEDCBA = d: HGFEDCBA
		// v                   = t
		// w:                  = 0
		p.t |= uint16(d)
		p.v = p.t
		p.w = 0
	}
}

func (p *PPU) writeData(d uint8) {
	p.vram.write8(p.v, d)
	if p.vramAddrInc32 {
		p.v += 32
	} else {
		p.v++
	}
}

func (p *PPU) readRegister(addr uint16) uint8 {
	if addr == 0x2002 {
		return p.status()
	} else if addr == 0x2007 {
		return p.readData()
	}
	log.Fatalf("PPU.readRegister 0x%X not yet supported", addr)
	return 0
}

func (p *PPU) readData() uint8 {
	d := p.vram.read8(p.v)

	if p.v%0x4000 < 0x3F00 {
		fmt.Printf("d was 0x%X, buffered was 0x%X\n", d, p.buffered)
		d, p.buffered = p.buffered, d
		fmt.Printf("d is 0x%X, buffered is 0x%X\n", d, p.buffered)
	} else {
		// FIXME
	}

	if p.vramAddrInc32 {
		p.v += 32
	} else {
		p.v++
	}

	return d
}

func (p *PPU) status() uint8 {
	var s uint8
	if p.spriteOverflow {
		s |= flagSpriteOverflow
	}
	if p.sprite0Hit {
		s |= flagSprite0Hit
	}
	if p.vblank {
		s |= flagVBlank
	}
	return s
}

func (p *PPU) step() {
	p.cycles++

	if p.cycles >= cyclesPerSL {
		p.cycles = 0
		p.sl++
		if p.sl >= slPerFrame {
			p.sl = 0
		}
	}

	if p.sl == vBlankSL && p.cycles == 1 {
		p.vblank = true
		p.invokeNMI()
	}

	if p.sl == preRenderLine && p.cycles == 1 {
		p.vblank = false
	}
}
