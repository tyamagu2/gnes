package gnes

// Add with Carry
// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
func adc(c *CPU, addr uint16, _ addrMode) {
	c.addWithCarry(c.a, c.read8(addr), c.c)
}

// AND
func and(c *CPU, addr uint16, _ addrMode) {
	c.a &= c.read8(addr)
	c.setZN(c.a)
}

// Arithmetic Shift Left
func asl(c *CPU, addr uint16, mode addrMode) {
	var was, v uint8
	if mode == acc {
		was = c.a
		v = was << 1
		c.a = v
	} else {
		was = c.read8(addr)
		v = was << 1
		c.write8(addr, v)
	}
	c.c = was >= 0x80
	c.setZN(v)
}

// BIT
func bit(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	c.z = v&c.a == 0
	c.v = v&(1<<6) != 0
	c.n = v&(1<<7) != 0
}

// Branch Instructions

// Branch on Plus
func bpl(c *CPU, addr uint16, _ addrMode) {
	if !c.n {
		c.pc = addr
	}
}

// Branch on Minus
func bmi(c *CPU, addr uint16, _ addrMode) {
	if c.n {
		c.pc = addr
	}
}

// Branch on Overflow Clear
func bvc(c *CPU, addr uint16, _ addrMode) {
	if !c.v {
		c.pc = addr
	}
}

// Branch on Overflow Set
func bvs(c *CPU, addr uint16, _ addrMode) {
	if c.v {
		c.pc = addr
	}
}

// Branch on Carry Clear
func bcc(c *CPU, addr uint16, _ addrMode) {
	if !c.c {
		c.pc = addr
	}
}

// Branch on Carry Set
func bcs(c *CPU, addr uint16, _ addrMode) {
	if c.c {
		c.pc = addr
	}
}

// Branch on Not Equal
func bne(c *CPU, addr uint16, _ addrMode) {
	if !c.z {
		c.pc = addr
	}
}

// Branch on Equal
func beq(c *CPU, addr uint16, _ addrMode) {
	if c.z {
		c.pc = addr
	}
}

// Break
func brk(c *CPU, _ uint16, _ addrMode) {
	c.stackPush16(c.pc)
	c.stackPush8(c.p() | flagB)
	c.i = true
	c.pc = c.read16(irqVector)
}

// Compare accumulator
func cmp(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.a, c.read8(addr))
}

// Compare X register
func cpx(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.x, c.read8(addr))
}

// Compare Y register
func cpy(c *CPU, addr uint16, _ addrMode) {
	c.compare(c.y, c.read8(addr))
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
	c.a ^= c.read8(addr)
	c.setZN(c.a)
}

// Flag (Processor Status) Instructions

// Clear Carry
func clc(c *CPU, _ uint16, _ addrMode) {
	c.c = false
}

// Set Carry
func sec(c *CPU, _ uint16, _ addrMode) {
	c.c = true
}

// Clear Intrrupt
func cli(c *CPU, _ uint16, _ addrMode) {
	c.i = false
}

// Set Intrrupt
func sei(c *CPU, _ uint16, _ addrMode) {
	c.i = true
}

// Clear Overflow
func clv(c *CPU, _ uint16, _ addrMode) {
	c.v = false
}

// Clear Decimal
func cld(c *CPU, _ uint16, _ addrMode) {
	c.d = false
}

// Set Decimal
func sed(c *CPU, _ uint16, _ addrMode) {
	c.d = true
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
	c.pc = addr
}

// Jump to Subroutine
func jsr(c *CPU, addr uint16, _ addrMode) {
	// JSR pushes the address-1 of the next operation on to the stack.
	ret := c.pc - 1
	c.stackPush16(ret)
	c.pc = addr
}

// Load Accumulator
func lda(c *CPU, addr uint16, _ addrMode) {
	c.a = c.read8(addr)
	c.setZN(c.a)
}

// Load X register
func ldx(c *CPU, addr uint16, _ addrMode) {
	c.x = c.read8(addr)
	c.setZN(c.x)
}

// Load Y register
func ldy(c *CPU, addr uint16, _ addrMode) {
	c.y = c.read8(addr)
	c.setZN(c.y)
}

// Logical Shift Right
func lsr(c *CPU, addr uint16, mode addrMode) {
	var was, v uint8
	if mode == acc {
		was = c.a
		v = was >> 1
		c.a = v
	} else {
		was = c.read8(addr)
		v = was >> 1
		c.write8(addr, v)
	}
	c.c = was&0x01 != 0
	c.setZN(v)
}

// NOP
func nop(_ *CPU, _ uint16, _ addrMode) {
}

// Bitwise OR with Accumulator
func ora(c *CPU, addr uint16, _ addrMode) {
	c.a |= c.read8(addr)
	c.setZN(c.a)
}

// Regiser Instructions

// Transfer A to X
func tax(c *CPU, _ uint16, _ addrMode) {
	c.x = c.a
	c.setZN(c.x)
}

// Transfer X to A
func txa(c *CPU, _ uint16, _ addrMode) {
	c.a = c.x
	c.setZN(c.a)
}

// Decrement X
func dex(c *CPU, _ uint16, _ addrMode) {
	c.x--
	c.setZN(c.x)
}

// Increment X
func inx(c *CPU, _ uint16, _ addrMode) {
	c.x++
	c.setZN(c.x)
}

// Transfer A to Y
func tay(c *CPU, _ uint16, _ addrMode) {
	c.y = c.a
	c.setZN(c.y)
}

// Transfer Y to A
func tya(c *CPU, _ uint16, _ addrMode) {
	c.a = c.y
	c.setZN(c.a)
}

// Decrement Y
func dey(c *CPU, _ uint16, _ addrMode) {
	c.y--
	c.setZN(c.y)
}

// Increment Y
func iny(c *CPU, _ uint16, _ addrMode) {
	c.y++
	c.setZN(c.y)
}

// Rotate Left
func rol(c *CPU, addr uint16, mode addrMode) {
	var v, was uint8
	if mode == acc {
		was = c.a
		v = c.rotateLeft(was)
		c.a = v
	} else {
		was = c.read8(addr)
		v = c.rotateLeft(was)
		c.write8(addr, v)
	}
	c.c = was >= 0x80
	c.setZN(v)
}

// Rotate Right
func ror(c *CPU, addr uint16, mode addrMode) {
	var v, was uint8
	if mode == acc {
		was = c.a
		v = c.rotateRight(was)
		c.a = v
	} else {
		was = c.read8(addr)
		v = c.rotateRight(was)
		c.write8(addr, v)
	}
	c.c = was&0x01 != 0
	c.setZN(v)
}

// Return from Interrupt
func rti(c *CPU, _ uint16, _ addrMode) {
	c.setProcessorStatus(c.stackPull8())
	c.pc = c.stackPull16()
}

// Return from Subroutine
func rts(c *CPU, _ uint16, _ addrMode) {
	c.pc = c.stackPull16() + 1
}

// Subtract with Carry
// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
func sbc(c *CPU, addr uint16, _ addrMode) {
	c.addWithCarry(c.a, ^c.read8(addr), c.c)
}

// Store Accumulator
func sta(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.a)
}

// Stack Instructions

// Transfer X to Stack Pointer
func txs(c *CPU, _ uint16, _ addrMode) {
	c.sp = c.x
}

// Transfer Stack Pointer to X
func tsx(c *CPU, _ uint16, _ addrMode) {
	c.x = c.sp
	c.setZN(c.x)
}

// Push Accumulator
func pha(c *CPU, _ uint16, _ addrMode) {
	c.stackPush8(c.a)
}

// Pull Accumulator
func pla(c *CPU, _ uint16, _ addrMode) {
	c.a = c.stackPull8()
	c.setZN(c.a)
}

// Push Processor Status
func php(c *CPU, _ uint16, _ addrMode) {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PHP sets P register value with B flag set
	c.stackPush8(c.p() | flagB)
}

// Pull Processor Status
func plp(c *CPU, _ uint16, _ addrMode) {
	c.setProcessorStatus(c.stackPull8())
}

// Store X Register
func stx(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.x)
}

// Store Y Register
func sty(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.y)
}

// Illegal instructions

// Decrement & Compare
func dcp(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr) - 1
	c.write8(addr, v)
	c.compare(c.a, v)
}

// INC + SBC (Increment & Subtract wiht Carry)
func isc(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr) + 1
	c.write8(addr, v)
	c.addWithCarry(c.a, ^v, c.c)
}

// LDA + LDX (Load Accumulator & X)
func lax(c *CPU, addr uint16, _ addrMode) {
	v := c.read8(addr)
	c.a = v
	c.x = v
	c.setZN(v)
}

// ROL + AND (Rotate Left & AND)
func rla(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	v := c.rotateLeft(was)
	c.write8(addr, v)
	c.c = was >= 0x80
	c.a &= v
	c.setZN(c.a)
}

// ROR + ADC (Rotate Left & Add with Carry)
func rra(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	v := c.rotateRight(was)
	c.write8(addr, v)
	c.c = was&0x01 != 0
	c.addWithCarry(c.a, v, c.c)
}

// Store Accumulator & X
func sax(c *CPU, addr uint16, _ addrMode) {
	c.write8(addr, c.a&c.x)
}

// ASL + ORA (Arithmetic Shift Left & Bitwise OR with Accumulator)
func slo(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	c.c = was >= 0x80
	v := was << 1
	c.write8(addr, v)
	c.a |= v
	c.setZN(c.a)
}

// LSR + EOR (Logical Shift Right & Exclusive OR with Accumulator
func sre(c *CPU, addr uint16, _ addrMode) {
	was := c.read8(addr)
	c.c = was&0x01 != 0
	v := was >> 1
	c.write8(addr, v)
	c.a ^= v
	c.setZN(c.a)
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

	c.a = uint8(v & 0xFF)
	// Set V flag if adding values of the same sign results in the opposite sign value
	c.v = (m^n)&0x80 == 0 && (m^c.a)&0x80 != 0
	c.c = v > 0xFF
	c.setZN(c.a)
}

// Compare the target register value agains the operand value and set flags
func (c *CPU) compare(r, o uint8) {
	c.c = r >= o
	w := r - o
	c.setZN(w)
}

func (c *CPU) rotateLeft(was uint8) uint8 {
	v := was << 1
	if c.c {
		v |= 0x01
	}
	return v
}

func (c *CPU) rotateRight(was uint8) uint8 {
	v := was >> 1
	if c.c {
		v |= 0x80
	}
	return v
}
