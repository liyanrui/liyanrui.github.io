package main

import (
	"fmt"
	"os"
	"io"
)

func main() {
	fmt.Println("beginning")
	input, _ := os.Open("/tmp/demo.ttt")
	fmt.Println("/tmp/demo.ttt opened!")
	data, _ := io.ReadAll(input)
	fmt.Printf("%d bytes /tmp/demo.ttt", len(data))
	input.Close()
}
