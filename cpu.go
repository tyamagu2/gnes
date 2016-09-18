package gnes

import (
	"fmt"
	"log"
)

const (
	StackBase   = 0x0100
	NMIVector   = 0xfffa
	ResetVector = 0xfffc
	IRQVector   = 0xfffe
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
)

var addrModes = []AddrMode{
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	abs, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, imp, imm, ind, abs, abs, abs,
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
	} else if mode == ind {
		fmt.Printf(" ($%2X%2X)", operands[1], operands[0])
	} else if mode == imm {
		fmt.Printf(" #$%2X", operands[0])
	} else if mode == rel {
		fmt.Printf(" *%2X", int8(operands[0]))
	} else if mode == izx {
		fmt.Printf(" ($%2X,X)", operands[0])
	} else if mode == izy {
		fmt.Printf(" ($%2X),Y", operands[0])
	}
	if mode == zpg || mode == imp || mode == rel {
		fmt.Printf("\t")
	}
	fmt.Printf("\tA:%2X X:%2X, Y:%2X P:%2X SP:%2X, CYC:TBD\n", c.A, c.X, c.Y, c.P(), c.SP)
}

// http://www.oxyron.de/html/opcodes02.html
var mnemonic = [256]string{
	"BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO",
	"PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO",
	"BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO",
	"CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO",
	"JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA",
	"PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA",
	"BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA",
	"SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA",
	"RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE",
	"PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE",
	"BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE",
	"CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE",
	"RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA",
	"PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA",
	"BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA",
	"SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA",
	"NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX",
	"DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX",
	"BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX",
	"TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",
	"LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX",
	"TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX",
	"BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX",
	"CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX",
	"CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP",
	"INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",
	"BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP",
	"CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP",
	"CPX", "SBC", "NOP", "ISC", "CPX", "SBC", "INC", "ISC",
	"INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC",
	"BEQ", "SBC", "KIL", "ISC", "NOP", "SBC", "INC", "ISC",
	"SED", "SBC", "NOP", "ISC", "NOP", "SBC", "INC", "ISC",
}

var numOperands = [256]uint8{
	0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	2, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 0, 2, 0, 0,
	1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 2, 2, 2, 0,
	1, 1, 0, 0, 1, 1, 1, 0, 0, 2, 0, 0, 2, 2, 2, 0,
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
	B   bool   // Break Command
	u   bool   // Always Set
	V   bool   // Overflow Flag
	N   bool   // Negative Flag
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
		c.PC += 1

		var addr uint16
		mode := addrModes[op]

		if mode == zpg {
			addr = c.addrZpg()
			c.PC += 1
		} else if mode == zpx {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 1
		} else if mode == zpy {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 1
		} else if mode == abs {
			addr = c.addrAbs()
			c.PC += 2
		} else if mode == abx {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 2
		} else if mode == aby {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 2
		} else if mode == ind {
			addr = c.addrInd()
			c.PC += 2
		} else if mode == imm {
			addr = c.addrImm()
			c.PC += 1
		} else if mode == izx {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 1
		} else if mode == izy {
			log.Fatalf("Unsupported mode %v\n", mode)
			c.PC += 1
		}

		if op == 0x08 {
			c.php()
		} else if op == 0x10 {
			c.bpl()
		} else if op == 0x18 {
			c.clc()
		} else if op == 0x20 {
			c.jsr(addr)
		} else if op == 0x21 || op == 0x25 || op == 0x29 || op == 0x2D || op == 0x31 || op == 0x35 || op == 0x39 || op == 0x3D {
			c.and(addr)
		} else if op == 0x24 || op == 0x2C {
			c.bit(addr)
		} else if op == 0x28 {
			c.plp()
		} else if op == 0x38 {
			c.sec()
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
		} else if op == 0x68 {
			c.pla()
		} else if op == 0x70 {
			c.bvs()
		} else if op == 0x78 {
			c.sei()
		} else if op == 0x86 {
			c.stx(addr)
		} else if op == 0x81 || op == 0x85 || op == 0x8D || op == 0x91 || op == 0x95 || op == 0x99 || op == 0x9D {
			c.sta(addr)
		} else if op == 0x90 {
			c.bcc()
		} else if op == 0x9A {
			c.txs()
		} else if op == 0xA1 || op == 0xA5 || op == 0xA9 || op == 0xAD || op == 0xB1 || op == 0xB5 || op == 0xB9 || op == 0xBD {
			c.lda(addr)
		} else if op == 0xA2 || op == 0xA6 || op == 0xAE || op == 0xB6 || op == 0xBE {
			c.ldx(addr)
		} else if op == 0xB0 {
			c.bcs()
		} else if op == 0xB8 {
			c.clv()
		} else if op == 0xC1 || op == 0xC5 || op == 0xC9 || op == 0xCD || op == 0xD1 || op == 0xD5 || op == 0xD9 || op == 0xDD {
			c.cmp(addr)
		} else if op == 0xD0 {
			c.bne()
		} else if op == 0xD8 {
			c.cld()
		} else if op == 0xEA {
			// NOP
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
	//c.PC = c.read16(ResetVector)
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
	c.B = flags&flagB != 0
	c.u = flags&flagU != 0
	c.V = flags&flagV != 0
	c.N = flags&flagN != 0
}

func (c *CPU) P() uint8 {
	var p uint8

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
	if c.B {
		p |= flagB
	}
	if c.u {
		p |= flagU
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

func (c *CPU) addrZpg() uint16 {
	return uint16(c.read8(c.PC))
}

func (c *CPU) addrImm() uint16 {
	return c.PC
}

func (c *CPU) addrAbs() uint16 {
	return c.read16(c.PC)
}

func (c *CPU) addrInd() uint16 {
	ref := c.read16(c.PC)
	return c.read16(ref)
}

func (c *CPU) addrRel(cond bool) uint16 {
	if cond {
		offset := uint16(c.read8(c.PC))
		// treat offset as signed
		if offset < 0x80 {
			return c.PC + 1 + offset
		} else {
			return c.PC + 1 + offset - 0x100
		}
	} else {
		return c.PC + 1
	}
}

// AND
func (c *CPU) and(addr uint16) {
	c.A &= c.read8(addr)
	c.Z = c.A == 0
	c.N = c.A >= 0x80
}

// Compare accumulator
func (c *CPU) cmp(addr uint16) {
	v := c.read8(addr)
	c.C = c.A >= v
	w := c.A - v
	c.Z = w == 0
	c.N = w >= 0x80
}

// Stack operations

func (c *CPU) stackPush(d uint8) {
	c.Mem.Write(StackBase+uint16(c.SP), d)
	c.SP--
}

func (c *CPU) stackPull() uint8 {
	c.SP++
	return c.read8(StackBase + uint16(c.SP))
}

// Jump
func (c *CPU) jmp(addr uint16) {
	c.PC = addr
}

// Jump to Subroutine
func (c *CPU) jsr(addr uint16) {
	// JSR pushes the address-1 of the next operation on to the stack.
	ret := c.PC - 1
	c.stackPush(uint8(ret >> 8))
	c.stackPush(uint8(ret & 0xff))
	c.PC = addr
}

// Load Accumulator
func (c *CPU) lda(addr uint16) {
	c.A = c.read8(addr)
	c.Z = c.A == 0
	c.N = c.A >= 0x80
}

// Store Accumulator
func (c *CPU) sta(addr uint16) {
	c.Mem.Write(addr, c.A)
}

// Load X register
func (c *CPU) ldx(addr uint16) {
	c.X = c.read8(addr)
	c.Z = c.X == 0
	c.N = c.X >= 0x80
}

// BIT
func (c *CPU) bit(addr uint16) {
	v := c.read8(addr)
	c.Z = v&c.A == 0
	c.V = v&(1<<6) != 0
	c.N = v&(1<<7) != 0
}

// Branch Instructions

// Branch on Plus
func (c *CPU) bpl() {
	c.PC = c.addrRel(!c.N)
}

// Branch on Minus
func (c *CPU) bmi() {
	c.PC = c.addrRel(c.N)
}

// Branch on Overflow Clear
func (c *CPU) bvc() {
	c.PC = c.addrRel(!c.V)
}

// Branch on Overflow Set
func (c *CPU) bvs() {
	c.PC = c.addrRel(c.V)
}

// Branch on Carry Clear
func (c *CPU) bcc() {
	c.PC = c.addrRel(!c.C)
}

// Branch on Carry Set
func (c *CPU) bcs() {
	c.PC = c.addrRel(c.C)
}

// Branch on Equal
func (c *CPU) beq() {
	c.PC = c.addrRel(c.Z)
}

// Branch on Not Equal
func (c *CPU) bne() {
	c.PC = c.addrRel(!c.Z)
}

// Flag (Processor Status) Instructions

// Clear Carry
func (c *CPU) clc() {
	c.C = false
}

// Set Carry
func (c *CPU) sec() {
	c.C = true
}

// Clear Intrrupt
func (c *CPU) cli() {
	c.I = false
}

// Set Intrrupt
func (c *CPU) sei() {
	c.I = true
}

// Clear Overflow
func (c *CPU) clv() {
	c.V = false
}

// Clear Decimal
func (c *CPU) cld() {
	c.D = false
}

// Set Decimal
func (c *CPU) sed() {
	c.D = true
}

// Return from Subroutine
func (c *CPU) rts() {
	lo := c.stackPull()
	hi := c.stackPull()
	addr := uint16(hi)<<8 | uint16(lo)
	c.PC = addr + 1
}

// Stack Instructions

// Transfer X to Stack ptr
func (c *CPU) txs() {
	c.SP = c.X
}

// Push Accumulator
func (c *CPU) pha() {
	c.stackPush(c.A)
}

// Pull Accumulator
func (c *CPU) pla() {
	c.A = c.stackPull()
	c.Z = c.A == 0
	c.N = c.A >= 0x80
}

// Push Processor Status
func (c *CPU) php() {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PHP sets B flag on push
	c.stackPush(c.P() | flagB)
}

// Pull Processor Status
func (c *CPU) plp() {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PLP ignores B flag
	c.setProcessorStatus(c.stackPull() & ^flagB)
}

// Store X Register
func (c *CPU) stx(addr uint16) {
	c.Mem.Write(addr, c.X)
}
