package gnes

import (
	"encoding/binary"
	"errors"
	"io"
	"os"
)

const (
	iNESMagic   = 0x4e45531a
	RPGBankSize = 16 << 10
	CHRBankSize = 8 << 10
	RAMBankSize = 8 << 10
)

type Mirroring int

const (
	Horizontal Mirroring = iota
	Vertical
)

type romHeader struct {
	Magic    uint32  // NES\x1a
	NumRPG   byte    // Number of 16kB RPG-ROM banks.
	NumCHR   byte    // Number of 8kB CHR-ROM/VROM banks.
	Control1 byte    // ROM Control Byte 1
	Control2 byte    // ROM Control Byte 2
	NumRAM   byte    // Number of 8kB RAM banks
	_        [7]byte // Reserved
}

type ROM struct {
	Mapper    byte
	NumRPG    byte
	RPG       []byte
	NumCHR    byte
	CHR       []byte
	NumRAM    int
	mirroring Mirroring
	Battery   bool
}

func LoadROM(path string) (*ROM, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	h := romHeader{}
	err = binary.Read(f, binary.BigEndian, &h)
	if err != nil {
		return nil, err
	}

	if h.Magic != iNESMagic {
		return nil, errors.New("Invalid ROM format")
	}

	if h.Control1&(1<<2) != 0 {
		return nil, errors.New("Trainer not supported")
	}

	rpg := make([]byte, RPGBankSize*int(h.NumRPG))
	if _, err := io.ReadFull(f, rpg); err != nil {
		return nil, err
	}

	chr := make([]byte, CHRBankSize*int(h.NumCHR))
	if _, err := io.ReadFull(f, chr); err != nil {
		return nil, err
	}

	rom := ROM{
		Mapper:  0xf0&h.Control2 | h.Control1>>4,
		NumRPG:  h.NumRPG,
		RPG:     rpg,
		NumCHR:  h.NumCHR,
		CHR:     chr,
		NumRAM:  int(h.NumRAM),
		Battery: h.Control1&(1<<1) != 0,
	}
	if rom.NumRAM == 0 {
		rom.NumRAM = 1
	}
	if h.Control1&1 == 0 {
		rom.mirroring = Horizontal
	} else {
		rom.mirroring = Vertical
	}

	return &rom, nil
}
