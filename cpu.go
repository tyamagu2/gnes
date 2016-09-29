package gnes

import "fmt"

const (
	stackBase   = 0x0100
	nmiVector   = 0xFFFA
	resetVector = 0xFFFC
	irqVector   = 0xFFFE
)

type addrMode uint8

// Addressing modes
const (
	_   addrMode = iota
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

func (m addrMode) String() string {
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
	}

	return "Unknown"
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

var instructions = []func(*CPU, uint16, addrMode){
	brk, ora, kil, slo, nop, ora, asl, slo, php, ora, asl, anc, nop, ora, asl, slo,
	bpl, ora, kil, slo, nop, ora, asl, slo, clc, ora, nop, slo, nop, ora, asl, slo,
	jsr, and, kil, rla, bit, and, rol, rla, plp, and, rol, anc, bit, and, rol, rla,
	bmi, and, kil, rla, nop, and, rol, rla, sec, and, nop, rla, nop, and, rol, rla,
	rti, eor, kil, sre, nop, eor, lsr, sre, pha, eor, lsr, alr, jmp, eor, lsr, sre,
	bvc, eor, kil, sre, nop, eor, lsr, sre, cli, eor, nop, sre, nop, eor, lsr, sre,
	rts, adc, kil, rra, nop, adc, ror, rra, pla, adc, ror, arr, jmp, adc, ror, rra,
	bvs, adc, kil, rra, nop, adc, ror, rra, sei, adc, nop, rra, nop, adc, ror, rra,
	nop, sta, nop, sax, sty, sta, stx, sax, dey, nop, txa, xaa, sty, sta, stx, sax,
	bcc, sta, kil, ahx, sty, sta, stx, sax, tya, sta, txs, tas, shy, sta, shx, ahx,
	ldy, lda, ldx, lax, ldy, lda, ldx, lax, tay, lda, tax, lax, ldy, lda, ldx, lax,
	bcs, lda, kil, lax, ldy, lda, ldx, lax, clv, lda, tsx, las, ldy, lda, ldx, lax,
	cpy, cmp, nop, dcp, cpy, cmp, dec, dcp, iny, cmp, dex, axs, cpy, cmp, dec, dcp,
	bne, cmp, kil, dcp, nop, cmp, dec, dcp, cld, cmp, nop, dcp, nop, cmp, dec, dcp,
	cpx, sbc, nop, isc, cpx, sbc, inc, isc, inx, sbc, nop, sbc, cpx, sbc, inc, isc,
	beq, sbc, kil, isc, nop, sbc, inc, isc, sed, sbc, nop, isc, nop, sbc, inc, isc,
}

var addrModes = []addrMode{
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	abs, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, abs, abs, abs, abs,
	rel, izy, imp, izy, zpx, zpx, zpx, zpx, imp, aby, imp, aby, abx, abx, abx, abx,
	imp, izx, imp, izx, zpg, zpg, zpg, zpg, imp, imm, acc, imm, ind, abs, abs, abs,
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
		fmt.Printf(" $%02X", operands[0])
	} else if mode == zpx {
		fmt.Printf(" $%02X,X", operands[0])
	} else if mode == zpy {
		fmt.Printf(" $%02X,Y", operands[0])
	} else if mode == abs {
		fmt.Printf(" $%02X%02X", operands[1], operands[0])
	} else if mode == abx {
		fmt.Printf(" $%02X%02X,X", operands[1], operands[0])
	} else if mode == aby {
		fmt.Printf(" $%02X%02X,Y", operands[1], operands[0])
	} else if mode == ind {
		fmt.Printf(" ($%02X%02X)", operands[1], operands[0])
	} else if mode == imm {
		fmt.Printf(" #$%02X", operands[0])
	} else if mode == acc {
		fmt.Printf(" A")
	} else if mode == rel {
		fmt.Printf(" *%02X", int8(operands[0]))
	} else if mode == izx {
		fmt.Printf(" ($%02X,X)", operands[0])
	} else if mode == izy {
		fmt.Printf(" ($%02X),Y", operands[0])
	}
	if mode == zpg || mode == imp || mode == acc || mode == rel {
		fmt.Printf("\t")
	}
	fmt.Printf("\t\t\tA:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:TBD\n", c.A, c.X, c.Y, c.P(), c.SP)
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

		// FIXME
		if c.PC == 0xC66E {
			return
		}

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
		} else if mode == rel {
			addr = c.addrRel()
			c.PC++
		} else if mode == izx {
			addr = c.addrIzx()
			c.PC++
		} else if mode == izy {
			addr = c.addrIzy()
			c.PC++
		}

		instructions[op](c, addr, mode)
	}
}

// http://wiki.nesdev.com/w/index.php/CPU_power_up_state
func (c *CPU) Reset() {
	c.PC = c.read16(resetVector)
	c.SP = 0xFD
	c.setProcessorStatus(0x24)
}

func (c *CPU) RunTest() {
	c.PC = 0xC000
	c.Run()
}

// Processor status flags
const (
	flagC uint8 = 1 << iota
	flagZ uint8 = 1 << iota
	flagI uint8 = 1 << iota
	flagD uint8 = 1 << iota
	flagB uint8 = 1 << iota
	flag5 uint8 = 1 << iota
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
	p := flag5 // always set 5th big
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

func (c *CPU) read16WrapAround(addr uint16) uint16 {
	lo := uint16(c.read8(addr))
	hi := uint16(c.read8((addr & 0xff00) | (addr+1)&0x00ff))
	return hi<<8 | lo
}

// Addressing Modes
// http://wiki.nesdev.com/w/index.php/CPU_addressing_modes

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
	return c.read16WrapAround(ref)
}

// Indexed Indirect

func (c *CPU) addrIzx() uint16 {
	ref := uint16(c.read8(c.PC) + c.X)
	return c.read16WrapAround(ref)
}

// Indirect Indexed

func (c *CPU) addrIzy() uint16 {
	ref := uint16(c.read8(c.PC))
	return c.read16WrapAround(ref) + uint16(c.Y)
}

// Relative

func (c *CPU) addrRel() uint16 {
	offset := uint16(c.read8(c.PC))
	// treat offset as signed
	if offset < 0x80 {
		return c.PC + 1 + offset
	}
	return c.PC + 1 + offset - 0x100
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
