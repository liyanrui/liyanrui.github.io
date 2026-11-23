package main

import (
	"fmt"
	"os"
)

func main() {
        for {
                input, err := os.Open("/tmp/hi-pipe")
                if err != nil {
                	fmt.Fprintln(os.Stderr, err)
                	return
                }
                os.WriteFile("/tmp/hi.sig", []byte("!"), 0)
                input.Close()
        }
}