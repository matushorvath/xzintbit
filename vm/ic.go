package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
)

var mem []int

var ip = 0
var rb = 0

func resizeMem(addr int) {
	if addr >= len(mem) {
		oldSize := len(mem)
		newSize := oldSize

		for addr >= newSize {
			newSize *= 2
		}

		mem = append(mem, make([]int, newSize-oldSize)...)
	}
}

func getMem(addr int) int {
	resizeMem(addr)
	return mem[addr]
}

func setMem(addr, val int) {
	resizeMem(addr)
	mem[addr] = val
}

var modeMul = []int{100, 1000, 10000}

func getParam(idx int) (int, error) {
	mode := getMem(ip) / modeMul[idx] % 10
	switch mode {
	case 0: // position mode
		return getMem(getMem(ip + idx + 1)), nil
	case 1: // immediate mode
		return getMem(ip + idx + 1), nil
	case 2: // relative mode
		return getMem(rb + getMem(ip + idx + 1)), nil
	default:
		return 0, fmt.Errorf("mode error: ip %d idx %d", ip, idx)
	}
}

func setParam(idx, val int) error {
	mode := getMem(ip) / modeMul[idx] % 10
	switch mode {
	case 0: // position mode
		setMem(getMem(ip + idx + 1), val)
		return nil
	case 2: // relative mode
		setMem(rb + getMem(ip + idx + 1), val)
		return nil
	default:
		return fmt.Errorf("mode error: ip %d idx %d", ip, idx)
	}
}

func run(input <-chan int, output chan<- int) error {
	defer close(output)

	for {
		oc := getMem(ip) % 100

		//fmt.Println(":", oc)

		switch oc {
		case 1: // add
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			p1, err := getParam(1)
			if err != nil {
				return err
			}
			setParam(2, p0 + p1)
			ip += 4

		case 2: // mul
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			p1, err := getParam(1)
			if err != nil {
				return err
			}
			err = setParam(2, p0 * p1)
			if err != nil {
				return err
			}
			ip += 4

		case 3: // in
			value, ok := <-input
			if !ok {
				return fmt.Errorf("no more inputs")
			}

			err := setParam(0, value)
			if err != nil {
				return err
			}
			ip += 2

		case 4: // out
			value, err := getParam(0)
			if err != nil {
				return err
			}
			output <- value
			ip += 2

		case 5: // jnz
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			if p0 != 0 {
				p1, err := getParam(1)
				if err != nil {
					return err
				}
				ip = p1
			} else {
				ip += 3
			}

		case 6: // jz
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			if p0 == 0 {
				p1, err := getParam(1)
				if err != nil {
					return err
				}
				ip = p1
			} else {
				ip += 3
			}

		case 7: // lt
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			p1, err := getParam(1)
			if err != nil {
				return err
			}

			var p2 int
			if p0 < p1 {
				p2 = 1
			} else {
				p2 = 0
			}
			err = setParam(2, p2)
			if err != nil {
				return err
			}
			ip += 4

		case 8: // eq
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			p1, err := getParam(1)
			if err != nil {
				return err
			}

			var p2 int
			if p0 == p1 {
				p2 = 1
			} else {
				p2 = 0
			}
			err = setParam(2, p2)
			if err != nil {
				return err
			}
			ip += 4

		case 9: // arb
			p0, err := getParam(0)
			if err != nil {
				return err
			}
			rb += p0
			ip += 2

		case 99: // hlt
			return nil

		default:
			return fmt.Errorf("opcode error: ip %d oc %d", ip, oc)
		}
	}
}

func getInput(input chan<- int) error {
	defer close(input)

	for {
		data := make([]byte, 1)
		n, err := os.Stdin.Read(data)
		for _, d := range data[0:n] {
			input <- int(d)
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
	}

	return nil
}

func setOutput(output <-chan int) {
	for {
		value, ok := <-output
		if !ok {
			break
		}
		fmt.Printf("%c", rune(value))
	}
}

func main() {
	mem = make([]int, 64)

	f, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatalf("%v\n", err)
	}
	data := strings.Split(strings.TrimSpace(string(f)), ",")

	for index, item := range data {
		num, err := strconv.Atoi(item)
		if err != nil {
			log.Fatalf("parse error: %v\n", err)
		}

		setMem(index, num)
	}

	input := make(chan int, 1)
	output := make(chan int, 1)

	// TODO error handling
	go run(input, output)
	go getInput(input)
	setOutput(output)
}
