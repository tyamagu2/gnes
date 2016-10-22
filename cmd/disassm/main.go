package main

import (
	"fmt"
	"log"
	"os"

	"github.com/tyamagu2/gnes"
)

func main() {
	rom, err := gnes.LoadROM(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	addr := 0
	size := len(rom.RPG)
	for addr < size {
		opcode := rom.RPG[addr]

		if addr+int(gnes.NumOperands[opcode]) >= size {
			return
		}

		operands := rom.RPG[addr+1 : addr+1+int(gnes.NumOperands[opcode])]
		mode := gnes.AddrModes[opcode]

		fmt.Printf("%4X %2X", 0xc000+addr, opcode)
		for _, operand := range operands {
			fmt.Printf(" %2X", operand)
		}
		if len(operands) == 0 {
			fmt.Printf("\t")
		}
		fmt.Printf("\t%s", gnes.Mnemonic[opcode])
		if mode == gnes.ZPG {
			fmt.Printf(" $%02X", operands[0])
		} else if mode == gnes.ZPX {
			fmt.Printf(" $%02X,X", operands[0])
		} else if mode == gnes.ZPY {
			fmt.Printf(" $%02X,Y", operands[0])
		} else if mode == gnes.ABS {
			fmt.Printf(" $%02X%02X", operands[1], operands[0])
		} else if mode == gnes.ABX {
			fmt.Printf(" $%02X%02X,X", operands[1], operands[0])
		} else if mode == gnes.ABY {
			fmt.Printf(" $%02X%02X,Y", operands[1], operands[0])
		} else if mode == gnes.IND {
			fmt.Printf(" ($%02X%02X)", operands[1], operands[0])
		} else if mode == gnes.IMM {
			fmt.Printf(" #$%02X", operands[0])
		} else if mode == gnes.ACC {
			fmt.Printf(" A")
		} else if mode == gnes.REL {
			fmt.Printf(" *%02X", int8(operands[0]))
		} else if mode == gnes.IZX {
			fmt.Printf(" ($%02X,X)", operands[0])
		} else if mode == gnes.IZY {
			fmt.Printf(" ($%02X),Y", operands[0])
		}
		if mode == gnes.ZPG || mode == gnes.IMP || mode == gnes.ACC || mode == gnes.REL {
			fmt.Printf("\t")
		}
		fmt.Printf("\n")
		addr += 1 + int(gnes.NumOperands[opcode])
	}
}
