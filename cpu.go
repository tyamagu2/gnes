package gnes

import "fmt"

const (
	stackBase   = 0x0100
	nmiVector   = 0xFFFA
	resetVector = 0xFFFC
	irqVector   = 0xFFFE
)

const (
	none = iota
	nmi
	irq
	reset
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

// Number of clock cycles used by each instruction
var cycles = []uint64{
	7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
}

// Number of extra clock cycles when page boundary crossed
var extraCycles = []uint64{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
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

var numOperands = [256]uint16{
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
	mem       *Memory
	pc        uint16 // Program Counter
	sp        uint8  // Stack Pointer
	a         uint8  // Accumulator
	x         uint8  // Index Register X
	y         uint8  // Index Register Y
	c         bool   // Carry Flag
	z         bool   // Zero Flag
	i         bool   // Interrupt Disable
	d         bool   // Decimal Mode
	v         bool   // Overflow Flag
	n         bool   // Negative Flag
	cycles    uint64
	interrupt uint8
}

func (c *CPU) printState() {
	opcode := c.read8(c.pc)
	operands := c.mem.readBytes(c.pc+1, uint8(numOperands[opcode]))
	mode := addrModes[opcode]

	fmt.Printf("%4X %2X", c.pc, opcode)
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
	fmt.Printf("\t\t\tA:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d\n", c.a, c.x, c.y, c.p(), c.sp, (c.cycles*3)%341)
}

func NewCPU(mem *Memory) *CPU {
	cpu := CPU{mem: mem}
	cpu.reset()
	return &cpu
}

func (c *CPU) step() uint64 {
	if c.interrupt == nmi {
		c.nmi()
	}
	c.interrupt = none

	c.printState()

	op := c.read8(c.pc)

	var addr uint16
	pageCrossed := false
	mode := addrModes[op]

	if mode == zpg {
		addr, pageCrossed = c.addrZpg()
	} else if mode == zpx {
		addr, pageCrossed = c.addrZpx()
	} else if mode == zpy {
		addr, pageCrossed = c.addrZpy()
	} else if mode == abs {
		addr, pageCrossed = c.addrAbs()
	} else if mode == abx {
		addr, pageCrossed = c.addrAbx()
	} else if mode == aby {
		addr, pageCrossed = c.addrAby()
	} else if mode == ind {
		addr, pageCrossed = c.addrInd()
	} else if mode == imm {
		addr, pageCrossed = c.addrImm()
	} else if mode == rel {
		addr, pageCrossed = c.addrRel()
	} else if mode == izx {
		addr, pageCrossed = c.addrIzx()
	} else if mode == izy {
		addr, pageCrossed = c.addrIzy()
	}

	c.pc += 1 + numOperands[op]

	instructions[op](c, addr, mode)

	cyclesWas := c.cycles
	c.cycles += cycles[op]
	if pageCrossed {
		c.cycles += extraCycles[op]
	}
	return c.cycles - cyclesWas
}

func NewTestCPU(rom *ROM) *CPU {
	mem := &Memory{rom: rom}
	cpu := CPU{mem: mem}
	cpu.reset()
	return &cpu
}

func (c *CPU) RunTest() {
	c.pc = 0xC000
	for {
		c.printState()

		op := c.read8(c.pc)

		var addr uint16
		pageCrossed := false
		mode := addrModes[op]

		if mode == zpg {
			addr, pageCrossed = c.addrZpg()
		} else if mode == zpx {
			addr, pageCrossed = c.addrZpx()
		} else if mode == zpy {
			addr, pageCrossed = c.addrZpy()
		} else if mode == abs {
			addr, pageCrossed = c.addrAbs()
		} else if mode == abx {
			addr, pageCrossed = c.addrAbx()
		} else if mode == aby {
			addr, pageCrossed = c.addrAby()
		} else if mode == ind {
			addr, pageCrossed = c.addrInd()
		} else if mode == imm {
			addr, pageCrossed = c.addrImm()
		} else if mode == rel {
			addr, pageCrossed = c.addrRel()
		} else if mode == izx {
			addr, pageCrossed = c.addrIzx()
		} else if mode == izy {
			addr, pageCrossed = c.addrIzy()
		}

		if c.pc == 0xC66E {
			return
		}

		c.pc += 1 + numOperands[op]

		instructions[op](c, addr, mode)

		c.cycles += cycles[op]
		if pageCrossed {
			c.cycles += extraCycles[op]
		}

	}
}

// http://wiki.nesdev.com/w/index.php/CPU_power_up_state
func (c *CPU) reset() {
	c.pc = c.read16(resetVector)
	c.sp = 0xFD
	c.setProcessorStatus(0x24)
}

func (c *CPU) invokeNMI() {
	c.interrupt = nmi
}

func (c *CPU) nmi() {
	c.stackPush16(c.pc)
	php(c, 0, 0)
	c.pc = c.read16(nmiVector)
}

// Processor status flags
const (
	flagC = 1 << iota
	flagZ
	flagI
	flagD
	flagB
	flag5
	flagV
	flagN
)

func (c *CPU) setProcessorStatus(flags uint8) {
	c.c = flags&flagC != 0
	c.z = flags&flagZ != 0
	c.i = flags&flagI != 0
	c.d = flags&flagD != 0
	c.v = flags&flagV != 0
	c.n = flags&flagN != 0
}

func (c *CPU) p() uint8 {
	var p uint8 = flag5 // always set 5th bit
	if c.c {
		p |= flagC
	}
	if c.z {
		p |= flagZ
	}
	if c.i {
		p |= flagI
	}
	if c.d {
		p |= flagD
	}
	if c.v {
		p |= flagV
	}
	if c.n {
		p |= flagN
	}
	return p
}

// Given the result of last instruction, sets Z and N flags
func (c *CPU) setZN(r uint8) {
	c.z = r == 0
	c.n = r >= 0x80
}

func (c *CPU) read8(addr uint16) uint8 {
	return c.mem.read(addr)
}

func (c *CPU) read16(addr uint16) uint16 {
	return uint16(c.read8(addr+1))<<8 | uint16(c.read8(addr))
}

func (c *CPU) read16WrapAround(addr uint16) uint16 {
	lo := uint16(c.read8(addr))
	hi := uint16(c.read8((addr & 0xff00) | (addr+1)&0x00ff))
	return hi<<8 | lo
}

func (c *CPU) write8(addr uint16, v uint8) {
	// TODO: refactor
	if addr == 0x4014 {
		// OAM DMA. Suspend CPU
		if c.cycles%2 == 0 {
			c.cycles += 513
		} else {
			c.cycles += 514
		}
	}
	c.mem.write(addr, v)
}

// Stack operations

func (c *CPU) stackPush8(v uint8) {
	c.write8(stackBase+uint16(c.sp), v)
	c.sp--
}

func (c *CPU) stackPush16(v uint16) {
	c.stackPush8(uint8(v >> 8))
	c.stackPush8(uint8(v & 0xff))
}

func (c *CPU) stackPull8() uint8 {
	c.sp++
	return c.read8(stackBase + uint16(c.sp))
}

func (c *CPU) stackPull16() uint16 {
	lo := c.stackPull8()
	hi := c.stackPull8()
	return uint16(hi)<<8 | uint16(lo)
}

// Addressing Modes
// http://wiki.nesdev.com/w/index.php/CPU_addressing_modes

// Zero Page
func (c *CPU) addrZpg() (uint16, bool) {
	return uint16(c.read8(c.pc + 1)), false
}

// Indexed Zero Page
// Wraparound is used in addition so that the address will always be in zero page.
// http://www.6502.org/tutorials/6502opcodes.html#WRAP

func (c *CPU) addrZpx() (uint16, bool) {
	return uint16(c.read8(c.pc+1) + c.x), false
}

func (c *CPU) addrZpy() (uint16, bool) {
	return uint16(c.read8(c.pc+1) + c.y), false
}

// Immediate
func (c *CPU) addrImm() (uint16, bool) {
	return c.pc + 1, false
}

// Absolute
func (c *CPU) addrAbs() (uint16, bool) {
	return c.read16(c.pc + 1), false
}

// Absolute Indexed
func (c *CPU) addrAbx() (uint16, bool) {
	base := c.read16(c.pc + 1)
	addr := base + uint16(c.x)
	return addr, pageCrossed(base, addr)
}

func (c *CPU) addrAby() (uint16, bool) {
	base := c.read16(c.pc + 1)
	addr := base + uint16(c.y)
	return addr, pageCrossed(base, addr)
}

// Indirect
func (c *CPU) addrInd() (uint16, bool) {
	ref := c.read16(c.pc + 1)
	return c.read16WrapAround(ref), false
}

// Indexed Indirect
func (c *CPU) addrIzx() (uint16, bool) {
	ref := uint16(c.read8(c.pc+1) + c.x)
	return c.read16WrapAround(ref), false
}

// Indirect Indexed
func (c *CPU) addrIzy() (uint16, bool) {
	ref := uint16(c.read8(c.pc + 1))
	base := c.read16WrapAround(ref)
	addr := base + uint16(c.y)
	return addr, pageCrossed(base, addr)
}

// Relative
func (c *CPU) addrRel() (uint16, bool) {
	offset := uint16(c.read8(c.pc + 1))
	// treat offset as signed
	if offset < 0x80 {
		return c.pc + 2 + offset, false
	}
	return c.pc + 2 + offset - 0x100, false
}

func pageCrossed(a, b uint16) bool {
	return a&0xFF00 != b&0xFF00
}
