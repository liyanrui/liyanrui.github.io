package main

import (
	"os"
	"fmt"
	"io"
)

func main() {
	fmt.Println("开始")
	input, _ := os.Open("/tmp/demo.pipe")
	scanner := os.NewScanner(input)
	for scanner.Scan() {
		fmt.Printf("收到：%s\n", scanner.Text())
	}
	data, _ := io.ReadAll(input)
	fmt.Println("管道数据接收完毕，收到 %d 字节", len(data))
	input.Close()
	fmt.Println("结束")
}
