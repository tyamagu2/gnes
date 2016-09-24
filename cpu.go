package gnes

import (
	"fmt"
	"log"
)

const (
	stackBase   = 0x0100
	nmiVector   = 0xFFFA
	resetVector = 0xFFFC
	irqVector   = 0xFFFE
)

type AddrMode uint8

// Addressing modes
const (
	_   AddrMode = iota
	zpg          // zeroPage
	zpx          // zeroPageX
	zpy          // zeroPageY
	abs          // absolute
	abx          // absoluteX
	aby          // absoluteY
	ind          // indirect
	imp          // implied
	acc          // accumulator
	imm          // immediate
	rel          // relative
	izx          // indexedIndirect
	izy          // indirectIndexed
	inj          // Indirect for JMP Indirect ($6C)
)

func (m AddrMode) String() string {
	if m == zpg {
		return "Zero Page"
	} else if m == zpx {
		return "Zero Page X"
	} else if m == zpy {
		return "Zero Page Y"
	} else if m == abs {
		return "Absolute"
	} else if m == abx {
		return "Absolute X"
	} else if m == aby {
		return "Absolute Y"
	} else if m == ind {
		return "Indirect"
	} else if m == imp {
		return "Implied"
	} else if m == acc {
		return "Accumulator"
	} else if m == imm {
		return "Immediate"
	} else if m == rel {
		return "Relative"
	} else if m == izx {
		return "Indexed Indirect (Indirect X)"
	} else if m == izy {
		return "Indirect Indexed (Indirect Y)"
	} else if m == inj {
		return "Indirect for JMP Indirect (%$C)"
	}

	return "Unknown"
}

var addrModes = []AddrMode{
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	abs, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, inj, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imm, izx, imm, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpy, zpy, imp, aby, imp, aby, abx, abx, aby, aby,
	imm, izx, imm, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpy, zpy, imp, aby, imp, aby, abx, abx, aby, aby,
	imm, izx, imm, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imm, izx, imm, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
}

// http://www.oxyron.de/html/opcodes02.html
var mnemonic = [256]string{
	"BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO",
	"BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO",
	"JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA", "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA",
	"BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA", "SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA",
	"RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE",
	"BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE",
	"RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA",
	"BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA",
	"NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX", "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX",
	"BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX", "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",
	"LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX", "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX",
	"BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX", "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX",
	"CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",
	"BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP", "CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP",
	"CPX", "SBC", "NOP", "ISC", "CPX", "SBC", "INC", "ISC", "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC",
	"BEQ", "SBC", "KIL", "ISC", "NOP", "SBC", "INC", "ISC", "SED", "SBC", "NOP", "ISC", "NOP", "SBC", "INC", "ISC",
}

var numOperands = [256]uint8{
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	2, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
}

type CPU struct {
	Mem *Memory
	PC  uint16 // Program Counter
	SP  uint8  // Stack Pointer
	A   uint8  // Accumulator
	X   uint8  // Index Register X
	Y   uint8  // Index Register Y
	C   bool   // Carry Flag
	Z   bool   // Zero Flag
	I   bool   // Interrupt Disable
	D   bool   // Decimal Mode
	V   bool   // Overflow Flag
	N   bool   // Negative Flag
}

func (c *CPU) printState() {
	opcode := c.read8(c.PC)
	operands := c.Mem.readBytes(c.PC+1, numOperands[opcode])
	mode := addrModes[opcode]

	fmt.Printf("%4X %2X", c.PC, opcode)
	for _, operand := range operands {
		fmt.Printf(" %2X", operand)
	}
	if len(operands) == 0 {
		fmt.Printf("\t")
	}
	fmt.Printf("\t%s", mnemonic[opcode])
	if mode == zpg {
		fmt.Printf(" $%2X", operands[0])
	} else if mode == zpx {
		fmt.Printf(" $%2X,X", operands[0])
	} else if mode == zpy {
		fmt.Printf(" $%2X,Y", operands[0])
	} else if mode == abs {
		fmt.Printf(" $%2X%2X", operands[1], operands[0])
	} else if mode == abx {
		fmt.Printf(" $%2X%2X,X", operands[1], operands[0])
	} else if mode == aby {
		fmt.Printf(" $%2X%2X,Y", operands[1], operands[0])
	} else if mode == ind || mode == inj {
		fmt.Printf(" ($%2X%2X)", operands[1], operands[0])
	} else if mode == imm {
		fmt.Printf(" #$%2X", operands[0])
	} else if mode == acc {
		fmt.Printf(" A")
	} else if mode == rel {
		fmt.Printf(" *%2X", int8(operands[0]))
	} else if mode == izx {
		fmt.Printf(" ($%2X,X)", operands[0])
	} else if mode == izy {
		fmt.Printf(" ($%2X),Y", operands[0])
	}
	if mode == zpg || mode == imp || mode == acc || mode == rel {
		fmt.Printf("\t")
	}
	fmt.Printf("\t\t\tA:%2X X:%2X Y:%2X P:%2X SP:%2X CYC:TBD\n", c.A, c.X, c.Y, c.P(), c.SP)
}

func NewCPU(rom *ROM) *CPU {
	mem := &Memory{ROM: rom}
	cpu := CPU{Mem: mem}
	cpu.Reset()
	return &cpu
}

func (c *CPU) Run() {
	for {
		c.printState()

		op := c.read8(c.PC)
		c.PC++

		var addr uint16
		mode := addrModes[op]

		if mode == zpg {
			addr = c.addrZpg()
			c.PC++
		} else if mode == zpx {
			addr = c.addrZpx()
			c.PC++
		} else if mode == zpy {
			addr = c.addrZpy()
			c.PC++
		} else if mode == abs {
			addr = c.addrAbs()
			c.PC += 2
		} else if mode == abx {
			addr = c.addrAbx()
			c.PC += 2
		} else if mode == aby {
			addr = c.addrAby()
			c.PC += 2
		} else if mode == ind {
			addr = c.addrInd()
			c.PC += 2
		} else if mode == imm {
			addr = c.addrImm()
			c.PC++
		} else if mode == izx {
			addr = c.addrIzx()
			c.PC++
		} else if mode == izy {
			addr = c.addrIzy()
			c.PC++
		} else if mode == inj {
			addr = c.addrInj()
			c.PC += 2
		}

		if op == 0x00 {
			c.brk()
		} else if op == 0x01 || op == 0x05 || op == 0x09 || op == 0x0D || op == 0x11 || op == 0x15 || op == 0x19 || op == 0x1D {
			c.ora(addr)
		} else if op == 0x03 || op == 0x07 || op == 0x0F || op == 0x13 || op == 0x17 || op == 0x1B || op == 0x1F {
			c.slo()
		} else if op == 0x06 || op == 0x0A || op == 0x0E || op == 0x16 || op == 0x1E {
			c.asl(addr, mode)
		} else if op == 0x08 {
			c.php()
		} else if op == 0x10 {
			c.bpl()
		} else if op == 0x18 {
			c.clc()
		} else if op == 0x20 {
			c.jsr(addr)
		} else if op == 0x21 || op == 0x25 || op == 0x29 || op == 0x2D || op == 0x31 || op == 0x35 || op == 0x39 || op == 0x3D {
			c.and(addr)
		} else if op == 0x23 || op == 0x27 || op == 0x2F || op == 0x33 || op == 0x37 || op == 0x3B || op == 0x3F {
			c.rla()
		} else if op == 0x24 || op == 0x2C {
			c.bit(addr)
		} else if op == 0x26 || op == 0x2A || op == 0x2E || op == 0x36 || op == 0x3E {
			c.rol(addr, mode)
		} else if op == 0x28 {
			c.plp()
		} else if op == 0x30 {
			c.bmi()
		} else if op == 0x38 {
			c.sec()
		} else if op == 0x40 {
			c.rti()
		} else if op == 0x41 || op == 0x45 || op == 0x49 || op == 0x4D || op == 0x51 || op == 0x55 || op == 0x59 || op == 0x5D {
			c.eor(addr)
		} else if op == 0x43 || op == 0x47 || op == 0x4F || op == 0x53 || op == 0x57 || op == 0x5B || op == 0x5F {
			c.sre()
		} else if op == 0x46 || op == 0x4A || op == 0x4E || op == 0x56 || op == 0x5E {
			c.lsr(addr, mode)
		} else if op == 0x48 {
			c.pha()
		} else if op == 0x4C || op == 0x6C {
			c.jmp(addr)
		} else if op == 0x50 {
			c.bvc()
		} else if op == 0x58 {
			c.cli()
		} else if op == 0x60 {
			c.rts()
		} else if op == 0x61 || op == 0x65 || op == 0x69 || op == 0x6D || op == 0x71 || op == 0x75 || op == 0x79 || op == 0x7D {
			c.adc(addr)
		} else if op == 0x63 || op == 0x67 || op == 0x6F || op == 0x73 || op == 0x77 || op == 0x7B || op == 0x7F {
			c.rra()
		} else if op == 0x66 || op == 0x6A || op == 0x6E || op == 0x76 || op == 0x7E {
			c.ror(addr, mode)
		} else if op == 0x68 {
			c.pla()
		} else if op == 0x70 {
			c.bvs()
		} else if op == 0x78 {
			c.sei()
		} else if op == 0x86 || op == 0x96 || op == 0x8E {
			c.stx(addr)
		} else if op == 0x81 || op == 0x85 || op == 0x8D || op == 0x91 || op == 0x95 || op == 0x99 || op == 0x9D {
			c.sta(addr)
		} else if op == 0x83 || op == 0x87 || op == 0x8F || op == 0x97 {
			c.sax(addr)
		} else if op == 0x84 || op == 0x8C || op == 0x94 {
			c.sty(addr)
		} else if op == 0x88 {
			c.dey()
		} else if op == 0x8A {
			c.txa()
		} else if op == 0x90 {
			c.bcc()
		} else if op == 0x98 {
			c.tya()
		} else if op == 0x9A {
			c.txs()
		} else if op == 0xA0 || op == 0xA4 || op == 0xAC || op == 0xB4 || op == 0xBC {
			c.ldy(addr)
		} else if op == 0xA1 || op == 0xA5 || op == 0xA9 || op == 0xAD || op == 0xB1 || op == 0xB5 || op == 0xB9 || op == 0xBD {
			c.lda(addr)
		} else if op == 0xA2 || op == 0xA6 || op == 0xAE || op == 0xB6 || op == 0xBE {
			c.ldx(addr)
		} else if op == 0xA3 || op == 0xA7 || op == 0xAF || op == 0xB3 || op == 0xB7 || op == 0xBF {
			c.lax(addr)
		} else if op == 0xA8 {
			c.tay()
		} else if op == 0xAA {
			c.tax()
		} else if op == 0xB0 {
			c.bcs()
		} else if op == 0xB8 {
			c.clv()
		} else if op == 0xBA {
			c.tsx()
		} else if op == 0xC0 || op == 0xC4 || op == 0xCC {
			c.cpy(addr)
		} else if op == 0xC1 || op == 0xC5 || op == 0xC9 || op == 0xCD || op == 0xD1 || op == 0xD5 || op == 0xD9 || op == 0xDD {
			c.cmp(addr)
		} else if op == 0xC6 || op == 0xCE || op == 0xD6 || op == 0xDE {
			c.dec(addr)
		} else if op == 0xC3 || op == 0xC7 || op == 0xCF || op == 0xD3 || op == 0xD7 || op == 0xDB || op == 0xDF {
			c.dcp(addr)
		} else if op == 0xC8 {
			c.iny()
		} else if op == 0xCA {
			c.dex()
		} else if op == 0xD0 {
			c.bne()
		} else if op == 0xD8 {
			c.cld()
		} else if op == 0xE0 || op == 0xE4 || op == 0xEC {
			c.cpx(addr)
		} else if op == 0xE1 || op == 0xE5 || op == 0xE9 || op == 0xED || op == 0xF1 || op == 0xF5 || op == 0xF9 || op == 0xFD || op == 0xEB { // EB not supported in 6502
			c.sbc(addr)
		} else if op == 0xE3 || op == 0xE7 || op == 0xEF || op == 0xF3 || op == 0xF7 || op == 0xFB || op == 0xFF {
			c.isc(addr)
		} else if op == 0xE6 || op == 0xEE || op == 0xF6 || op == 0xFE {
			c.inc(addr)
		} else if op == 0xE8 {
			c.inx()
		} else if op == 0x04 || op == 0x0C || op == 0x14 || op == 0x1A || op == 0x1C || op == 0x34 || op == 0x3A || op == 0x3C || op == 0x44 || op == 0x54 || op == 0x5A || op == 0x5C || op == 0x64 || op == 0x74 || op == 0x7A || op == 0x7C || op == 0x80 || op == 0xD4 || op == 0xDA || op == 0xDC || op == 0xEA || op == 0xF4 || op == 0xFA || op == 0xFC {
			c.nop()
		} else if op == 0xF0 {
			c.beq()
		} else if op == 0xF8 {
			c.sed()
		} else {
			log.Fatalf("0x%x not supported yet.", op)
		}
	}
}

// http://wiki.nesdev.com/w/index.php/CPU_power_up_state
func (c *CPU) Reset() {
	// FIXME
	//c.PC = c.read16(resetVector)
	c.PC = 0xc000
	c.SP = 0xfd
	c.A = 0
	c.X = 0
	c.Y = 0
	c.setProcessorStatus(0x24)
}

// Processor status flags
const (
	flagC uint8 = 1 << iota
	flagZ uint8 = 1 << iota
	flagI uint8 = 1 << iota
	flagD uint8 = 1 << iota
	flagB uint8 = 1 << iota
	flagU uint8 = 1 << iota
	flagV uint8 = 1 << iota
	flagN uint8 = 1 << iota
)

func (c *CPU) setProcessorStatus(flags uint8) {
	c.C = flags&flagC != 0
	c.Z = flags&flagZ != 0
	c.I = flags&flagI != 0
	c.D = flags&flagD != 0
	c.V = flags&flagV != 0
	c.N = flags&flagN != 0
}

func (c *CPU) P() uint8 {
	p := flagU // always set U

	if c.C {
		p |= flagC
	}
	if c.Z {
		p |= flagZ
	}
	if c.I {
		p |= flagI
	}
	if c.D {
		p |= flagD
	}
	if c.V {
		p |= flagV
	}
	if c.N {
		p |= flagN
	}
	return p
}

func (c *CPU) read8(addr uint16) uint8 {
	return c.Mem.Read(addr)
}

func (c *CPU) read16(addr uint16) uint16 {
	return uint16(c.read8(addr+1))<<8 | uint16(c.read8(addr))
}

// Zero Page

func (c *CPU) addrZpg() uint16 {
	return uint16(c.read8(c.PC))
}

// Indexed Zero Page
// Wraparound is used in addition so that the address will always be in zero page.
// http://www.6502.org/tutorials/6502opcodes.html#WRAP

func (c *CPU) addrZpx() uint16 {
	return uint16(c.read8(c.PC) + c.X)
}

func (c *CPU) addrZpy() uint16 {
	return uint16(c.read8(c.PC) + c.Y)
}

// Immediate

func (c *CPU) addrImm() uint16 {
	return c.PC
}

// Absolute

func (c *CPU) addrAbs() uint16 {
	return c.read16(c.PC)
}

// Absolute Indexed

func (c *CPU) addrAbx() uint16 {
	return c.read16(c.PC) + uint16(c.X)
}

func (c *CPU) addrAby() uint16 {
	return c.read16(c.PC) + uint16(c.Y)
}

// Indirect
func (c *CPU) addrInd() uint16 {
	ref := c.read16(c.PC)
	return c.read16(ref)
}

// Indexed Indirect

func (c *CPU) addrIzx() uint16 {
	ref := uint16(c.read8(c.PC) + c.X)
	return c.read16(ref)
}

// Indirect Indexed

func (c *CPU) addrIzy() uint16 {
	ref := uint16(c.read8(c.PC))
	return c.read16(ref) + uint16(c.Y)
}

// Indirect for JMP Indirect ($6C)

func (c *CPU) addrInj() uint16 {
	ref := c.read16(c.PC)
	lo := uint16(c.read8(ref))
	ha := (ref & 0xff00) | (ref+1)&0x00ff
	hi := uint16(c.read8(ha))
	return hi<<8 | lo
}

func (c *CPU) addrRel(cond bool) uint16 {
	if cond {
		offset := uint16(c.read8(c.PC))
		// treat offset as signed
		if offset < 0x80 {
			return c.PC + 1 + offset
		}
		return c.PC + 1 + offset - 0x100
	}
	return c.PC + 1
}

// Stack operations

func (c *CPU) stackPush8(v uint8) {
	c.Mem.Write(stackBase+uint16(c.SP), v)
	c.SP--
}

func (c *CPU) stackPush16(v uint16) {
	c.stackPush8(uint8(v >> 8))
	c.stackPush8(uint8(v & 0xff))
}

func (c *CPU) stackPull8() uint8 {
	c.SP++
	return c.read8(stackBase + uint16(c.SP))
}

func (c *CPU) stackPull16() uint16 {
	lo := c.stackPull8()
	hi := c.stackPull8()
	return uint16(hi)<<8 | uint16(lo)
}

// Given the result of last instruction, sets Z and N flags
func (c *CPU) setZN(r uint8) {
	c.Z = r == 0
	c.N = r >= 0x80
}
