package gnes

import "image/color"

// ref: http://nesdev.com/NESDoc.pdf Appendix F
var color_palette = color.Palette{
	color.RGBA{0x75, 0x75, 0x75, 0xFF},
	color.RGBA{0x27, 0x1B, 0x8F, 0xFF},
	color.RGBA{0x00, 0x00, 0xAB, 0xFF},
	color.RGBA{0x47, 0x00, 0x9F, 0xFF},
	color.RGBA{0x8F, 0x00, 0x77, 0xFF},
	color.RGBA{0xAB, 0x00, 0x13, 0xFF},
	color.RGBA{0xA7, 0x00, 0x00, 0xFF},
	color.RGBA{0x7F, 0x0B, 0x00, 0xFF},
	color.RGBA{0x43, 0x2F, 0x00, 0xFF},
	color.RGBA{0x00, 0x47, 0x00, 0xFF},
	color.RGBA{0x00, 0x51, 0x00, 0xFF},
	color.RGBA{0x00, 0x3F, 0x17, 0xFF},
	color.RGBA{0x1B, 0x3F, 0x5F, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0xBC, 0xBC, 0xBC, 0xFF},
	color.RGBA{0x00, 0x73, 0xEF, 0xFF},
	color.RGBA{0x23, 0x3B, 0xEF, 0xFF},
	color.RGBA{0x83, 0x00, 0xF3, 0xFF},
	color.RGBA{0xBF, 0x00, 0xBF, 0xFF},
	color.RGBA{0xE7, 0x00, 0x5B, 0xFF},
	color.RGBA{0xDB, 0x2B, 0x00, 0xFF},
	color.RGBA{0xCB, 0x4F, 0x0F, 0xFF},
	color.RGBA{0x8B, 0x73, 0x00, 0xFF},
	color.RGBA{0x00, 0x97, 0x00, 0xFF},
	color.RGBA{0x00, 0xAB, 0x00, 0xFF},
	color.RGBA{0x00, 0x93, 0x3B, 0xFF},
	color.RGBA{0x00, 0x83, 0x8B, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0xFF, 0xFF, 0xFF, 0xFF},
	color.RGBA{0x3F, 0xBF, 0xFF, 0xFF},
	color.RGBA{0x5F, 0x97, 0xFF, 0xFF},
	color.RGBA{0xA7, 0x8B, 0xFD, 0xFF},
	color.RGBA{0xF7, 0x7B, 0xFF, 0xFF},
	color.RGBA{0xFF, 0x77, 0xB7, 0xFF},
	color.RGBA{0xFF, 0x77, 0x63, 0xFF},
	color.RGBA{0xFF, 0x9B, 0x3B, 0xFF},
	color.RGBA{0xF3, 0xBF, 0x3F, 0xFF},
	color.RGBA{0x83, 0xD3, 0x13, 0xFF},
	color.RGBA{0x4F, 0xDF, 0x4B, 0xFF},
	color.RGBA{0x58, 0xF8, 0x98, 0xFF},
	color.RGBA{0x00, 0xEB, 0xDB, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0xFF, 0xFF, 0xFF, 0xFF},
	color.RGBA{0xAB, 0xE7, 0xFF, 0xFF},
	color.RGBA{0xC7, 0xD7, 0xFF, 0xFF},
	color.RGBA{0xD7, 0xCB, 0xFF, 0xFF},
	color.RGBA{0xFF, 0xC7, 0xFF, 0xFF},
	color.RGBA{0xFF, 0xC7, 0xDB, 0xFF},
	color.RGBA{0xFF, 0xBF, 0xB3, 0xFF},
	color.RGBA{0xFF, 0xDB, 0xAB, 0xFF},
	color.RGBA{0xFF, 0xE7, 0xA3, 0xFF},
	color.RGBA{0xE3, 0xFF, 0xA3, 0xFF},
	color.RGBA{0xAB, 0xF3, 0xBF, 0xFF},
	color.RGBA{0xB3, 0xFF, 0xCF, 0xFF},
	color.RGBA{0x9F, 0xFF, 0xF3, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0x00, 0xFF},
}