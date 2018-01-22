// crawlpaper - "wallpapers" of rougelike text interfaces
//
// Usage:
//   go get github.com/fogleman/gg
//   go build crawlpaper.go
//   ./crawlpaper < crawl.paper

package main

import (
	"bufio"
	"github.com/fogleman/gg"
	"os"
)

func main() {
	// TWEAK width, height
	dc := gg.NewContext(1600, 1200)

	// TWEAK font path, font size
	err := dc.LoadFontFace(os.Getenv("HOME")+"/.fonts/NotoMono-Regular.ttf", 40)
	if err != nil {
		panic(err)
	}

	// TWEAK background and foreground colors
	dc.SetRGB(1, 1, 1)
	dc.Clear()
	dc.SetRGB(0, 0, 0)

	input := bufio.NewScanner(os.Stdin)
	i := 0
	for input.Scan() {
		// TWEAK where to put the individual lines TODO might be easier
		// to use text wrapping facility of gg library?
		dc.DrawString(input.Text(), 42, 48+48*float64(i))
		i++
	}

	dc.SavePNG("crawlpaper.png")
}
