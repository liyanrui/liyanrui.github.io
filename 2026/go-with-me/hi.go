package main

import (
        "fmt"
        "os"
        "io"
        "strings"
        "bufio"
	"math/rand"
	"time"
)

var routers = make(map[string]func(chan string, string))
func handle_func(path string, handler func(chan string, string)) {
        routers[path] = handler
}
func foo(w chan string, r string) {
	a := rand.New(rand.NewSource(time.Now().UnixNano()))
	time.Sleep(time.Duration(a.Intn(5)) * time.Second)
        w <- fmt.Sprintf("对于 %s，我认为烟涛微茫信难求！", r)
}
func observer() {
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

func main() {
        go observer()
        handle_func("/", foo)
        _safe([[input, err := /os.Open/("/tmp/hi-pipe")]])
	response := make(chan string)
        for {
                _safe([[sig, err := /os.Open/("/tmp/hi.sig")]])
                io.ReadAll(sig)
                sig.Close()
                scanner := bufio.NewScanner(input)
		n := 0
                for scanner.Scan() {
                        url := scanner.Text()
                        i := strings.Index(url, "/")
                        if i < 0 {
                                continue
                        }
                        request := url[i:]
                        if routers[request] != nil {
                                go routers[request](response, request)
                        } else {
                                go routers["/"](response, request)
                        }
                }
		for i:= 0; i < a; i++ {
			result := <- response
			fmt.Println(result)
		}
        }
        input.Close()
}
