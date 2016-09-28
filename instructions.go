package gnes

// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
// Add with Carry
func adc(c *CPU, addr uint16, mode AddrMode) {
	c.addWithCarry(c.A, c.read8(addr), c.C)
}

// Subtract with Carry
func sbc(c *CPU, addr uint16, mode AddrMode) {
	c.addWithCarry(c.A, ^c.read8(addr), c.C)
}

// AND
func and(c *CPU, addr uint16, mode AddrMode) {
	c.A &= c.read8(addr)
	c.setZN(c.A)
}

// Arithmetic Shift Left
func asl(c *CPU, addr uint16, mode AddrMode) {
	var was, v uint8
	if mode == acc {
		was = c.A
		v = was << 1
		c.A = v
	} else {
		was = c.read8(addr)
		v = was << 1
		c.Mem.Write(addr, v)
	}
	c.C = was >= 0x80
	c.setZN(v)
}

// Break
func brk(c *CPU, _ uint16, _ AddrMode) {
	c.stackPush16(c.PC)
	c.stackPush8(c.P() | flagB)
	c.I = true
	c.PC = c.read16(irqVector)
}

// Compare accumulator
func cmp(c *CPU, addr uint16, mode AddrMode) {
	c.compare(c.A, c.read8(addr))
}

// Compare X register
func cpx(c *CPU, addr uint16, mode AddrMode) {
	c.compare(c.X, c.read8(addr))
}

// Compare Y register
func cpy(c *CPU, addr uint16, mode AddrMode) {
	c.compare(c.Y, c.read8(addr))
}

// Bitwise Exclusive Or
func eor(c *CPU, addr uint16, mode AddrMode) {
	c.A ^= c.read8(addr)
	c.setZN(c.A)
}

// Increment memory
func inc(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr)
	v++
	c.Mem.Write(addr, v)
	c.setZN(v)
}

// Jump
func jmp(c *CPU, addr uint16, mode AddrMode) {
	c.PC = addr
}

// Jump to Subroutine
func jsr(c *CPU, addr uint16, mode AddrMode) {
	// JSR pushes the address-1 of the next operation on to the stack.
	ret := c.PC - 1
	c.stackPush16(ret)
	c.PC = addr
}

// Load Accumulator
func lda(c *CPU, addr uint16, mode AddrMode) {
	c.A = c.read8(addr)
	c.setZN(c.A)
}

// Store Accumulator
func sta(c *CPU, addr uint16, mode AddrMode) {
	c.Mem.Write(addr, c.A)
}

// Load X register
func ldx(c *CPU, addr uint16, mode AddrMode) {
	c.X = c.read8(addr)
	c.setZN(c.X)
}

// Load Y register
func ldy(c *CPU, addr uint16, mode AddrMode) {
	c.Y = c.read8(addr)
	c.setZN(c.Y)
}

// Logical Shift Right
func lsr(c *CPU, addr uint16, mode AddrMode) {
	var was, v uint8
	if mode == acc {
		was = c.A
		v = was >> 1
		c.A = v
	} else {
		was = c.read8(addr)
		v = was >> 1
		c.Mem.Write(addr, v)
	}
	c.C = was&0x01 != 0
	c.setZN(v)
}

// BIT
func bit(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr)
	c.Z = v&c.A == 0
	c.V = v&(1<<6) != 0
	c.N = v&(1<<7) != 0
}

// Branch Instructions

// Branch on Plus
func bpl(c *CPU, addr uint16, mode AddrMode) {
	if !c.N {
		c.PC = addr
	}
}

// Branch on Minus
func bmi(c *CPU, addr uint16, mode AddrMode) {
	if c.N {
		c.PC = addr
	}
}

// Branch on Overflow Clear
func bvc(c *CPU, addr uint16, mode AddrMode) {
	if !c.V {
		c.PC = addr
	}
}

// Branch on Overflow Set
func bvs(c *CPU, addr uint16, mode AddrMode) {
	if c.V {
		c.PC = addr
	}
}

// Branch on Carry Clear
func bcc(c *CPU, addr uint16, mode AddrMode) {
	if !c.C {
		c.PC = addr
	}
}

// Branch on Carry Set
func bcs(c *CPU, addr uint16, mode AddrMode) {
	if c.C {
		c.PC = addr
	}
}

// Branch on Equal
func beq(c *CPU, addr uint16, mode AddrMode) {
	if c.Z {
		c.PC = addr
	}
}

// Branch on Not Equal
func bne(c *CPU, addr uint16, mode AddrMode) {
	if !c.Z {
		c.PC = addr
	}
}

// Decrement memory
func dec(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr)
	v--
	c.Mem.Write(addr, v)
	c.setZN(v)
}

// Flag (Processor Status) Instructions

// Clear Carry
func clc(c *CPU, _ uint16, _ AddrMode) {
	c.C = false
}

// Set Carry
func sec(c *CPU, _ uint16, _ AddrMode) {
	c.C = true
}

// Clear Intrrupt
func cli(c *CPU, _ uint16, _ AddrMode) {
	c.I = false
}

// NOP
func nop(_ *CPU, _ uint16, _ AddrMode) {
}

// Bitwise OR with Accumulator
func ora(c *CPU, addr uint16, mode AddrMode) {
	c.A |= c.read8(addr)
	c.setZN(c.A)
}

// Regiser Instructions

// Transfer A to X
func tax(c *CPU, _ uint16, _ AddrMode) {
	c.X = c.A
	c.setZN(c.X)
}

// Transfer X to A
func txa(c *CPU, _ uint16, _ AddrMode) {
	c.A = c.X
	c.setZN(c.A)
}

// Decrement X
func dex(c *CPU, _ uint16, _ AddrMode) {
	c.X--
	c.setZN(c.X)
}

// Increment X
func inx(c *CPU, _ uint16, _ AddrMode) {
	c.X++
	c.setZN(c.X)
}

// Transfer A to Y
func tay(c *CPU, _ uint16, _ AddrMode) {
	c.Y = c.A
	c.setZN(c.Y)
}

// Transfer Y to A
func tya(c *CPU, _ uint16, _ AddrMode) {
	c.A = c.Y
	c.setZN(c.A)
}

// Decrement Y
func dey(c *CPU, _ uint16, _ AddrMode) {
	c.Y--
	c.setZN(c.Y)
}

// Increment Y
func iny(c *CPU, _ uint16, _ AddrMode) {
	c.Y++
	c.setZN(c.Y)
}

// Set Intrrupt
func sei(c *CPU, _ uint16, _ AddrMode) {
	c.I = true
}

// Clear Overflow
func clv(c *CPU, _ uint16, _ AddrMode) {
	c.V = false
}

// Clear Decimal
func cld(c *CPU, _ uint16, _ AddrMode) {
	c.D = false
}

// Set Decimal
func sed(c *CPU, _ uint16, _ AddrMode) {
	c.D = true
}

// Rotate Left
func rol(c *CPU, addr uint16, mode AddrMode) {
	var v, was uint8
	if mode == acc {
		was = c.A
		v = c.rotateLeft(was)
		c.A = v
	} else {
		was = c.read8(addr)
		v = c.rotateLeft(was)
		c.Mem.Write(addr, v)
	}
	c.C = was >= 0x80
	c.setZN(v)
}

// Rotate Right
func ror(c *CPU, addr uint16, mode AddrMode) {
	var v, was uint8
	if mode == acc {
		was = c.A
		v = c.rotateRight(was)
		c.A = v
	} else {
		was = c.read8(addr)
		v = c.rotateRight(was)
		c.Mem.Write(addr, v)
	}
	c.C = was&0x01 != 0
	c.setZN(v)
}

// Return from Interrupt
func rti(c *CPU, _ uint16, _ AddrMode) {
	c.setProcessorStatus(c.stackPull8())
	c.PC = c.stackPull16()
}

// Return from Subroutine
func rts(c *CPU, _ uint16, _ AddrMode) {
	c.PC = c.stackPull16() + 1
}

// Push Accumulator
func pha(c *CPU, _ uint16, _ AddrMode) {
	c.stackPush8(c.A)
}

// Pull Accumulator
func pla(c *CPU, _ uint16, _ AddrMode) {
	c.A = c.stackPull8()
	c.setZN(c.A)
}

// Push Processor Status
func php(c *CPU, _ uint16, _ AddrMode) {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PHP sets P register value with B flag set
	c.stackPush8(c.P() | flagB)
}

// Pull Processor Status
func plp(c *CPU, _ uint16, _ AddrMode) {
	c.setProcessorStatus(c.stackPull8())
}

// Stack Instructions

// Transfer X to Stack Pointer
func txs(c *CPU, _ uint16, _ AddrMode) {
	c.SP = c.X
}

// Transfer Stack Pointer to X
func tsx(c *CPU, _ uint16, _ AddrMode) {
	c.X = c.SP
	c.setZN(c.X)
}

// Store X Register
func stx(c *CPU, addr uint16, mode AddrMode) {
	c.Mem.Write(addr, c.X)
}

// Store Y Register
func sty(c *CPU, addr uint16, mode AddrMode) {
	c.Mem.Write(addr, c.Y)
}

// Illegal instructions

// Decrement & Compare
func dcp(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr) - 1
	c.Mem.Write(addr, v)
	c.compare(c.A, v)
}

// INC + SBC (Increment & Subtract wiht Carry)
func isc(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr) + 1
	c.Mem.Write(addr, v)
	c.addWithCarry(c.A, ^v, c.C)
}

// LDA + LDX (Load Accumulator & X)
func lax(c *CPU, addr uint16, mode AddrMode) {
	v := c.read8(addr)
	c.A = v
	c.X = v
	c.setZN(v)
}

// Store Accumulator & X
func sax(c *CPU, addr uint16, mode AddrMode) {
	c.Mem.Write(addr, c.A&c.X)
}

// ROL + AND (Rotate Left & AND)
func rla(c *CPU, addr uint16, mode AddrMode) {
	was := c.read8(addr)
	v := c.rotateLeft(was)
	c.Mem.Write(addr, v)
	c.C = was >= 0x80
	c.A &= v
	c.setZN(c.A)
}

// ROR + ADC (Rotate Left & Add with Carry)
func rra(c *CPU, addr uint16, mode AddrMode) {
	was := c.read8(addr)
	v := c.rotateRight(was)
	c.Mem.Write(addr, v)
	c.C = was&0x01 != 0
	c.addWithCarry(c.A, v, c.C)
}

// ASL + ORA (Arithmetic Shift Left & Bitwise OR with Accumulator)
func slo(c *CPU, addr uint16, mode AddrMode) {
	was := c.read8(addr)
	c.C = was >= 0x80
	v := was << 1
	c.Mem.Write(addr, v)
	c.A |= v
	c.setZN(c.A)
}

// LSR + EOR (Logical Shift Right & Exclusive OR with Accumulator
func sre(c *CPU, addr uint16, mode AddrMode) {
	was := c.read8(addr)
	c.C = was&0x01 != 0
	v := was >> 1
	c.Mem.Write(addr, v)
	c.A ^= v
	c.setZN(c.A)
}

// Unofficial & Unsupported Instructions

func ahx(_ *CPU, _ uint16, _ AddrMode) {}
func alr(_ *CPU, _ uint16, _ AddrMode) {}
func anc(_ *CPU, _ uint16, _ AddrMode) {}
func arr(_ *CPU, _ uint16, _ AddrMode) {}
func axs(_ *CPU, _ uint16, _ AddrMode) {}
func kil(_ *CPU, _ uint16, _ AddrMode) {}
func las(_ *CPU, _ uint16, _ AddrMode) {}
func shx(_ *CPU, _ uint16, _ AddrMode) {}
func shy(_ *CPU, _ uint16, _ AddrMode) {}
func tas(_ *CPU, _ uint16, _ AddrMode) {}
func xaa(_ *CPU, _ uint16, _ AddrMode) {}

// Helpers

func (c *CPU) addWithCarry(m, n uint8, carry bool) {
	v := int(m) + int(n)
	if carry {
		v++
	}

	c.A = uint8(v & 0xFF)
	// Set V flag if adding values of the same sign results in the opposite sign value
	c.V = (m^n)&0x80 == 0 && (m^c.A)&0x80 != 0
	c.C = v > 0xFF
	c.setZN(c.A)
}

// Compare the target register value agains the operand value and set flags
func (c *CPU) compare(r, o uint8) {
	c.C = r >= o
	w := r - o
	c.setZN(w)
}

func (c *CPU) rotateLeft(was uint8) uint8 {
	v := was << 1
	if c.C {
		v |= 0x01
	}
	return v
}

func (c *CPU) rotateRight(was uint8) uint8 {
	v := was >> 1
	if c.C {
		v |= 0x80
	}
	return v
}
