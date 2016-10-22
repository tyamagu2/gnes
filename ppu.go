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
	flagCtrlNN = 3
	flagCtrlI  = 1 << (2 + iota)
	flagCtrlS
	flagCtrlB
	flagCtrlH
	flagCtrlP
	flagCtrlV
)

const (
	flagGrayscale = 1 << iota
	flagShowBgLeftmost
	flagShowSpritesLeftmost
	flagShowBg
	flagShowSprites
	flagEmphasizeRed
	flagEmphasizeGreen
	flagEmphasizeBlue
)

const (
	flagSpriteOverflow = 1 << (5 + iota)
	flagSprite0Hit
	flagVBlank
)

// Picture Processing Unit
type PPU struct {
	cycles     int
	sl         int // Scan Line
	vram       *VRAM
	readBuffer uint8
	oam        [256]uint8 // Object Attribute Memory / Sprite RAM
	oamAddr    uint8

	// Registers
	v uint16 // Current VRAM address
	t uint16 // Temporary VRAM address
	x uint8  // Fine X scroll
	w uint8  // First or second write toggle

	// PPUCTRL
	ctrl uint8

	// PPUMASK
	grayscale           bool
	showBgLeftmost      bool
	showSpritesLeftmost bool
	showBg              bool
	showSprites         bool
	emphasizeRed        bool
	emphasizeGreen      bool
	emphasizeBlue       bool

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

func (p *PPU) registerAddr(addr uint16) uint16 {
	return 0x2000 + (addr-0x2000)%8
}

// Register read operations

func (p *PPU) readRegister(_addr uint16) uint8 {
	addr := p.registerAddr(_addr)
	switch addr {
	case 0x2002:
		return p.readStatus()
	case 0x2004:
		return p.readOamData()
	case 0x2007:
		return p.readData()
	}

	log.Fatalf("PPU.readRegister 0x%X not yet supported", addr)
	return 0
}

// Read $2002
func (p *PPU) readStatus() uint8 {
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

// Read $2004
func (p *PPU) readOamData() uint8 {
	return p.oam[p.oamAddr]
}

// Read $2007
func (p *PPU) readData() uint8 {
	d := p.vram.read8(p.v)

	if p.v%0x4000 < 0x3F00 {
		fmt.Printf("d was 0x%X, readBuffer was 0x%X\n", d, p.readBuffer)
		d, p.readBuffer = p.readBuffer, d
		fmt.Printf("d is 0x%X, readBuffer is 0x%X\n", d, p.readBuffer)
	} else {
		// FIXME
	}

	if p.ctrl&flagCtrlI != 0 {
		p.v += 32
	} else {
		p.v++
	}

	return d
}

// Register write operations

func (p *PPU) writeRegister(addr uint16, d uint8) {
	switch addr {
	case 0x2000:
		p.writeCtrl(d)
	case 0x2001:
		p.writeMask(d)
	case 0x2003:
		p.writeOamAddr(d)
	case 0x2004:
		p.writeOamData(d)
	case 0x2005:
		p.writeScroll(d)
	case 0x2006:
		p.writeAddr(d)
	case 0x2007:
		p.writeData(d)
	default:
		log.Fatalf("Write to 0x%X not yet supported\n", addr)
	}
}

// Write to $2000
func (p *PPU) writeCtrl(d uint8) {
	p.ctrl = d
	// t: ...BA.. ........ = d: ......BA
	p.t = (p.t & 0xF3FF) | (uint16(d&0x03) << 10)
}

// Write to $2001
func (p *PPU) writeMask(d uint8) {
	p.grayscale = d&flagGrayscale != 0
	p.showBgLeftmost = d&flagShowBgLeftmost != 0
	p.showSpritesLeftmost = d&flagShowSpritesLeftmost != 0
	p.showBg = d&flagShowBg != 0
	p.showSprites = d&flagShowSprites != 0
	p.emphasizeRed = d&flagEmphasizeRed != 0
	p.emphasizeGreen = d&flagEmphasizeGreen != 0
	p.emphasizeBlue = d&flagEmphasizeBlue != 0
}

// Write to $2003
func (p *PPU) writeOamAddr(d uint8) {
	p.oamAddr = d
}

// Write to $2004
func (p *PPU) writeOamData(d uint8) {
	p.oam[p.oamAddr] = d
	p.oamAddr++
}

// Write to $2005
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

// Write to $2006
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

// Write to $2007
func (p *PPU) writeData(d uint8) {
	p.vram.write8(p.v, d)
	if p.ctrl&flagCtrlI != 0 {
		p.v += 32
	} else {
		p.v++
	}
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