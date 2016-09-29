package gnes

const (
	stackBase   = 0x0100
	nmiVector   = 0xFFFA
	resetVector = 0xFFFC
	irqVector   = 0xFFFE
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
	V   bool   // Overflow Flag
	N   bool   // Negative Flag
}

func NewCPU(rom *ROM) *CPU {
	mem := &Memory{rom: rom}
	cpu := CPU{Mem: mem}
	cpu.Reset()
	return &cpu
}

// http://wiki.nesdev.com/w/index.php/CPU_power_up_state
func (c *CPU) Reset() {
	c.PC = c.read16(resetVector)
	c.SP = 0xFD
	c.setProcessorStatus(0x24)
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
	c.C = flags&flagC != 0
	c.Z = flags&flagZ != 0
	c.I = flags&flagI != 0
	c.D = flags&flagD != 0
	c.V = flags&flagV != 0
	c.N = flags&flagN != 0
}

func (c *CPU) P() uint8 {
	var p uint8 = flag5 // always set 5th bit
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
	return c.Mem.read(addr)
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
	c.Mem.write(addr, v)
}

// Stack operations

func (c *CPU) stackPush8(v uint8) {
	c.write8(stackBase+uint16(c.SP), v)
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

// Addressing Modes
// http://wiki.nesdev.com/w/index.php/CPU_addressing_modes

// Zero Page
func (c *CPU) addrZpg() uint16 {
	return uint16(c.read8(c.PC + 1))
}

// Indexed Zero Page
// Wraparound is used in addition so that the address will always be in zero page.
// http://www.6502.org/tutorials/6502opcodes.html#WRAP
func (c *CPU) addrZpx() uint16 {
	return uint16(c.read8(c.PC+1) + c.X)
}

func (c *CPU) addrZpy() uint16 {
	return uint16(c.read8(c.PC+1) + c.Y)
}

// Immediate
func (c *CPU) addrImm() uint16 {
	return c.PC + 1
}

// Absolute
func (c *CPU) addrAbs() uint16 {
	return c.read16(c.PC + 1)
}

// Absolute Indexed
func (c *CPU) addrAbx() uint16 {
	return c.read16(c.PC+1) + uint16(c.X)
}

func (c *CPU) addrAby() uint16 {
	return c.read16(c.PC+1) + uint16(c.Y)
}

// Indirect
func (c *CPU) addrInd() uint16 {
	ref := c.read16(c.PC + 1)
	return c.read16WrapAround(ref)
}

// Indexed Indirect
func (c *CPU) addrIzx() uint16 {
	ref := uint16(c.read8(c.PC+1) + c.X)
	return c.read16WrapAround(ref)
}

// Indirect Indexed
func (c *CPU) addrIzy() uint16 {
	ref := uint16(c.read8(c.PC + 1))
	return c.read16WrapAround(ref) + uint16(c.Y)
}

// Relative
func (c *CPU) addrRel() uint16 {
	offset := uint16(c.read8(c.PC + 1))
	// treat offset as signed
	if offset < 0x80 {
		return c.PC + 2 + offset
	}
	return c.PC + 2 + offset - 0x100
}
