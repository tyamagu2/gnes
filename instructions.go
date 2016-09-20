package gnes

// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
// Add with Carry
func (c *CPU) adc(addr uint16) {
	c.addWithCarry(c.A, c.read8(addr), c.C)
}

// Subtract with Carry
func (c *CPU) sbc(addr uint16) {
	c.addWithCarry(c.A, ^c.read8(addr), c.C)
}

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

// AND
func (c *CPU) and(addr uint16) {
	c.A &= c.read8(addr)
	c.setZN(c.A)
}

// Compare accumulator
func (c *CPU) cmp(addr uint16) {
	c.compare(c.A, c.read8(addr))
}

// Compare X register
func (c *CPU) cpx(addr uint16) {
	c.compare(c.X, c.read8(addr))
}

// Compare Y register
func (c *CPU) cpy(addr uint16) {
	c.compare(c.Y, c.read8(addr))
}

// Bitwise Exclusive Or
func (c *CPU) eor(addr uint16) {
	c.A ^= c.read8(addr)
	c.setZN(c.A)
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
	c.setZN(c.A)
}

// Store Accumulator
func (c *CPU) sta(addr uint16) {
	c.Mem.Write(addr, c.A)
}

// Load X register
func (c *CPU) ldx(addr uint16) {
	c.X = c.read8(addr)
	c.setZN(c.X)
}

// Load Y register
func (c *CPU) ldy(addr uint16) {
	c.Y = c.read8(addr)
	c.setZN(c.Y)
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

// NOP
func (c *CPU) nop() {
}

// Bitwise OR with Accumulator
func (c *CPU) ora(addr uint16) {
	c.A |= c.read8(addr)
	c.setZN(c.A)
}

// Regiser Instructions

// Increment Y
func (c *CPU) iny() {
	c.Y++
	c.setZN(c.Y)
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

// Push Accumulator
func (c *CPU) pha() {
	c.stackPush(c.A)
}

// Pull Accumulator
func (c *CPU) pla() {
	c.A = c.stackPull()
	c.setZN(c.A)
}

// Push Processor Status
func (c *CPU) php() {
	// http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
	// PHP sets P register value with B flag set
	c.stackPush(c.P() | flagB)
}

// Pull Processor Status
func (c *CPU) plp() {
	c.setProcessorStatus(c.stackPull())
}

// Stack Instructions

// Transfer X to Stack ptr
func (c *CPU) txs() {
	c.SP = c.X
}

// Store X Register
func (c *CPU) stx(addr uint16) {
	c.Mem.Write(addr, c.X)
}

// Store Y Register
func (c *CPU) sty(addr uint16) {
	c.Mem.Write(addr, c.Y)
}

// Helpers

// Compare the target register value agains the operand value and set flags
func (c *CPU) compare(r, o uint8) {
	c.C = r >= o
	w := r - o
	c.setZN(w)
}
