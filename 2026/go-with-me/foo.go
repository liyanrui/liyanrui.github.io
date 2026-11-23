package main

import (
	"os"
	"fmt"
	"io"
)

func main() {
	fmt.Println("开始")
	input, _ := os.Open("/tmp/demo.pipe")
	fmt.Println("等待接收管道数据")
	data, _ := io.ReadAll(input)
	fmt.Printf("管道数据接收完毕，收到 %d 字节\n", len(data))
	input.Close()
	fmt.Println("结束")
}
