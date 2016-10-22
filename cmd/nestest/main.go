package main

import (
	"log"
	"os"

	"github.com/tyamagu2/gnes"
)

func main() {
	rom, err := gnes.LoadROM(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	cpu := gnes.NewTestCPU(rom)
	cpu.RunTest()
}
