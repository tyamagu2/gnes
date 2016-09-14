package gnes

import (
	"fmt"
	"log"
)

const (
	StackBase   = 0x01ff
	NMIVector   = 0xfffa
	ResetVector = 0xfffc
	IRQVector   = 0xfffe
)

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
	V   bool   // Overflow Flag
	N   bool   // Negative Flag
}

func NewCPU(rom *ROM) *CPU {
	mem := &Memory{ROM: rom}
	cpu := CPU{Mem: mem}
	cpu.Reset()
	return &cpu
}

func (c *CPU) Show() {
	fmt.Println("---------")
	fmt.Printf("PC: 0x%x\n", c.PC)
	fmt.Printf("SP: 0x%x\n", c.SP)
	fmt.Printf("A : 0x%x\n", c.A)
	fmt.Printf("X : 0x%x\n", c.X)
	fmt.Printf("Y : 0x%x\n", c.Y)
	fmt.Printf("P :\n")
	fmt.Println("  C:", c.C)
	fmt.Println("  Z:", c.Z)
	fmt.Println("  I:", c.I)
	fmt.Println("  D:", c.D)
	fmt.Println("  B:", c.B)
	fmt.Println("  V:", c.V)
	fmt.Println("  N:", c.N)
}

func (c *CPU) Run() {
	for {
		c.Show()

		op := c.read8(c.PC)
		fmt.Printf("OP: 0x%x\n", op)
		c.PC += 1

		if op == 0x18 {
			c.clc()
		} else if op == 0x2c {
			c.bit(c.addrAbs())
		} else if op == 0x38 {
			c.sec()
		} else if op == 0x58 {
			c.cli()
		} else if op == 0x78 {
			c.sei()
		} else if op == 0x8d {
			c.sta(c.addrAbs())
		} else if op == 0x9a {
			c.txs()
		} else if op == 0xa2 {
			c.ldx(c.addrImm())
		} else if op == 0xa9 {
			c.lda(c.addrImm())
		} else if op == 0xad {
			c.lda(c.addrAbs())
		} else if op == 0xb8 {
			c.clv()
		} else if op == 0xd8 {
			c.cld()
		} else if op == 0xf8 {
			c.sed()
		} else {
			log.Fatalf("0x%x not supported yet.", op)
		}
	}
}

func (c *CPU) Reset() {
	c.PC = c.read16(ResetVector)
}

func (c *CPU) read8(addr uint16) uint8 {
	return c.Mem.Read(addr)
}

func (c *CPU) read16(addr uint16) uint16 {
	return uint16(c.Mem.Read(addr+1))<<8 | uint16(c.Mem.Read(addr))
}

func (c *CPU) addrImm() uint16 {
	c.PC += 1
	return c.PC - 1
}

func (c *CPU) addrAbs() uint16 {
	c.PC += 2
	return c.read16(c.PC - 2)
}

// Load Accumulator
func (c *CPU) lda(addr uint16) {
	c.A = c.read8(addr)
}

// Store Accumulator
func (c *CPU) sta(addr uint16) {
	c.Mem.Write(addr, c.A)
}

// Load X register
func (c *CPU) ldx(addr uint16) {
	c.X = c.read8(addr)
}

// Stack Instructions

// Transfer X to Stack ptr
func (c *CPU) txs() {
	c.SP = c.X
}

// BIT
func (c *CPU) bit(addr uint16) {
	v := c.read8(addr)
	c.Z = v&c.A != 0
	c.V = v&(1<<6) != 0
	c.N = v&(1<<7) != 0
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
