package main

import (
	"fmt"
	"os"
	"strconv"
)

func fib(n int) int {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	fmt.Printf("%d\n", fib(n))
}
