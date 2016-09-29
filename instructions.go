package gnes

// Add with Carry
// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
func adc(c *CPU, addr uint16, _ addrMode) {
	c.addWithCarry(c.A, c.read8(addr), c.C)
}

// AND
func and(c *CPU, addr uint16, _ addrMode) {
	c.A &= c.read8(addr)
	c.setZN(c.A)
}

// Arithmetic Shift Left
func asl(c *CPU, addr uint16, mode addrMode) {
	var was, v uint8
	if mode == acc {
		was = c.A
		v = was << 1
		c.A = v
	} else {
		was = c.read8(addr)
		v = was << 1
		c.write8(addr, v)
	}
	c.C = was >= 0x80
	c.setZN(v)
}

// BIT
func bit(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	c.Z = v&c.A == 0
	c.V = v&(1<<6) != 0
	c.N = v&(1<<7) != 0
}

// Branch Instructions

// Branch on Plus
func bpl(c *CPU, addr uint16, _ addrMode) {
	if !c.N {
		c.PC = addr
	}
}

// Branch on Minus
func bmi(c *CPU, addr uint16, _ addrMode) {
	if c.N {
		c.PC = addr
	}
}

// Branch on Overflow Clear
func bvc(c *CPU, addr uint16, _ addrMode) {
	if !c.V {
		c.PC = addr
	}
}

// Branch on Overflow Set
func bvs(c *CPU, addr uint16, _ addrMode) {
	if c.V {
		c.PC = addr
	}
}

// Branch on Carry Clear
func bcc(c *CPU, addr uint16, _ addrMode) {
	if !c.C {
		c.PC = addr
	}
}

// Branch on Carry Set
func bcs(c *CPU, addr uint16, _ addrMode) {
	if c.C {
		c.PC = addr
	}
}

// Branch on Not Equal
func bne(c *CPU, addr uint16, _ addrMode) {
	if !c.Z {
		c.PC = addr
	}
}

// Branch on Equal
func beq(c *CPU, addr uint16, _ addrMode) {
	if c.Z {
		c.PC = addr
	}
}

// Break
func brk(c *CPU, _ uint16, _ addrMode) {
	c.stackPush16(c.PC)
	c.stackPush8(c.P() | flagB)
	c.I = true
	c.PC = c.read16(irqVector)
}

// Compare accumulator
func cmp(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.A, c.read8(addr))
}

// Compare X register
func cpx(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.X, c.read8(addr))
}

// Compare Y register
func cpy(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.Y, c.read8(addr))
}

// Decrement memory
func dec(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	v--
	c.write8(addr, v)
	c.setZN(v)
}

// Bitwise Exclusive Or
func eor(c *CPU, addr uint16, _ addrMode) {
	c.A ^= c.read8(addr)
	c.setZN(c.A)
}

// Flag (Processor Status) Instructions

// Clear Carry
func clc(c *CPU, _ uint16, _ addrMode) {
	c.C = false
}

// Set Carry
func sec(c *CPU, _ uint16, _ addrMode) {
	c.C = true
}

// Clear Intrrupt
func cli(c *CPU, _ uint16, _ addrMode) {
	c.I = false
}

// Set Intrrupt
func sei(c *CPU, _ uint16, _ addrMode) {
	c.I = true
}

// Clear Overflow
func clv(c *CPU, _ uint16, _ addrMode) {
	c.V = false
}

// Clear Decimal
func cld(c *CPU, _ uint16, _ addrMode) {
	c.D = false
}

// Set Decimal
func sed(c *CPU, _ uint16, _ addrMode) {
	c.D = true
}

// Increment memory
func inc(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	v++
	c.write8(addr, v)
	c.setZN(v)
}

// Jump
func jmp(c *CPU, addr uint16, _ addrMode) {
	c.PC = addr
}

// Jump to Subroutine
func jsr(c *CPU, addr uint16, _ addrMode) {
	// JSR pushes the address-1 of the next operation on to the stack.
	ret := c.PC - 1
	c.stackPush16(ret)
	c.PC = addr
}

// Load Accumulator
func lda(c *CPU, addr uint16, _ addrMode) {
	c.A = c.read8(addr)
	c.setZN(c.A)
}

// Load X register
func ldx(c *CPU, addr uint16, _ addrMode) {
	c.X = c.read8(addr)
	c.setZN(c.X)
}

// Load Y register
func ldy(c *CPU, addr uint16, _ addrMode) {
	c.Y = c.read8(addr)
	c.setZN(c.Y)
}

// Logical Shift Right
func lsr(c *CPU, addr uint16, mode addrMode) {
	var was, v uint8
	if mode == acc {
		was = c.A
		v = was >> 1
		c.A = v
	} else {
		was = c.read8(addr)
		v = was >> 1
		c.write8(addr, v)
	}
	c.C = was&0x01 != 0
	c.setZN(v)
}

// NOP
func nop(_ *CPU, _ uint16, _ addrMode) {
}

// Bitwise OR with Accumulator
func ora(c *CPU, addr uint16, _ addrMode) {
	c.A |= c.read8(addr)
	c.setZN(c.A)
}

// Regiser Instructions

// Transfer A to X
func tax(c *CPU, _ uint16, _ addrMode) {
	c.X = c.A
	c.setZN(c.X)
}

// Transfer X to A
func txa(c *CPU, _ uint16, _ addrMode) {
	c.A = c.X
	c.setZN(c.A)
}

// Decrement X
func dex(c *CPU, _ uint16, _ addrMode) {
	c.X--
	c.setZN(c.X)
}

// Increment X
func inx(c *CPU, _ uint16, _ addrMode) {
	c.X++
	c.setZN(c.X)
}

// Transfer A to Y
func tay(c *CPU, _ uint16, _ addrMode) {
	c.Y = c.A
	c.setZN(c.Y)
}

// Transfer Y to A
func tya(c *CPU, _ uint16, _ addrMode) {
	c.A = c.Y
	c.setZN(c.A)
}

// Decrement Y
func dey(c *CPU, _ uint16, _ addrMode) {
	c.Y--
	c.setZN(c.Y)
}

// Increment Y
func iny(c *CPU, _ uint16, _ addrMode) {
	c.Y++
	c.setZN(c.Y)
}

// Rotate Left
func rol(c *CPU, addr uint16, mode addrMode) {
	var v, was uint8
	if mode == acc {
		was = c.A
		v = c.rotateLeft(was)
		c.A = v
	} else {
		was = c.read8(addr)
		v = c.rotateLeft(was)
		c.write8(addr, v)
	}
	c.C = was >= 0x80
	c.setZN(v)
}

// Rotate Right
func ror(c *CPU, addr uint16, mode addrMode) {
	var v, was uint8
	if mode == acc {
		was = c.A
		v = c.rotateRight(was)
		c.A = v
	} else {
		was = c.read8(addr)
		v = c.rotateRight(was)
		c.write8(addr, v)
	}
	c.C = was&0x01 != 0
	c.setZN(v)
}

// Return from Interrupt
func rti(c *CPU, _ uint16, _ addrMode) {
	c.setProcessorStatus(c.stackPull8())
	c.PC = c.stackPull16()
}

// Return from Subroutine
func rts(c *CPU, _ uint16, _ addrMode) {
	c.PC = c.stackPull16() + 1
}

// Subtract with Carry
// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
func sbc(c *CPU, addr uint16, _ addrMode) {
	c.addWithCarry(c.A, ^c.read8(addr), c.C)
}

// Store Accumulator
func sta(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.A)
}

// Stack Instructions

// Transfer X to Stack Pointer
func txs(c *CPU, _ uint16, _ addrMode) {
	c.SP = c.X
}

// Transfer Stack Pointer to X
func tsx(c *CPU, _ uint16, _ addrMode) {
	c.X = c.SP
	c.setZN(c.X)
}

// Push Accumulator
func pha(c *CPU, _ uint16, _ addrMode) {
	c.stackPush8(c.A)
}

// Pull Accumulator
func pla(c *CPU, _ uint16, _ addrMode) {
	c.A = c.stackPull8()
	c.setZN(c.A)
}

// Push Processor Status
func php(c *CPU, _ uint16, _ addrMode) {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PHP sets P register value with B flag set
	c.stackPush8(c.P() | flagB)
}

// Pull Processor Status
func plp(c *CPU, _ uint16, _ addrMode) {
	c.setProcessorStatus(c.stackPull8())
}

// Store X Register
func stx(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.X)
}

// Store Y Register
func sty(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.Y)
}

// Illegal instructions

// Decrement & Compare
func dcp(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr) - 1
	c.write8(addr, v)
	c.compare(c.A, v)
}

// INC + SBC (Increment & Subtract wiht Carry)
func isc(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr) + 1
	c.write8(addr, v)
	c.addWithCarry(c.A, ^v, c.C)
}

// LDA + LDX (Load Accumulator & X)
func lax(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	c.A = v
	c.X = v
	c.setZN(v)
}

// ROL + AND (Rotate Left & AND)
func rla(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	v := c.rotateLeft(was)
	c.write8(addr, v)
	c.C = was >= 0x80
	c.A &= v
	c.setZN(c.A)
}

// ROR + ADC (Rotate Left & Add with Carry)
func rra(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	v := c.rotateRight(was)
	c.write8(addr, v)
	c.C = was&0x01 != 0
	c.addWithCarry(c.A, v, c.C)
}

// Store Accumulator & X
func sax(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.A&c.X)
}

// ASL + ORA (Arithmetic Shift Left & Bitwise OR with Accumulator)
func slo(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	c.C = was >= 0x80
	v := was << 1
	c.write8(addr, v)
	c.A |= v
	c.setZN(c.A)
}

// LSR + EOR (Logical Shift Right & Exclusive OR with Accumulator
func sre(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	c.C = was&0x01 != 0
	v := was >> 1
	c.write8(addr, v)
	c.A ^= v
	c.setZN(c.A)
}

// Illegal & Unsupported Instructions

func ahx(_ *CPU, _ uint16, _ addrMode) {}
func alr(_ *CPU, _ uint16, _ addrMode) {}
func anc(_ *CPU, _ uint16, _ addrMode) {}
func arr(_ *CPU, _ uint16, _ addrMode) {}
func axs(_ *CPU, _ uint16, _ addrMode) {}
func kil(_ *CPU, _ uint16, _ addrMode) {}
func las(_ *CPU, _ uint16, _ addrMode) {}
func shx(_ *CPU, _ uint16, _ addrMode) {}
func shy(_ *CPU, _ uint16, _ addrMode) {}
func tas(_ *CPU, _ uint16, _ addrMode) {}
func xaa(_ *CPU, _ uint16, _ addrMode) {}

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
