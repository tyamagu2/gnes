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

	fmt.Println("Mapper:", rom.Mapper)
	fmt.Println("RAM:", rom.NumRAM)
	fmt.Println("Battery:", rom.Battery)
	fmt.Println(err)

	cpu := gnes.NewCPU(rom)
	cpu.Run()
}
