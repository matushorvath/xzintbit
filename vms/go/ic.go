package main

import (
	"fmt"
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

func binInst(op func(int, int) int) error {
	p0, err := getParam(0)
	if err != nil {
		return err
	}

	p1, err := getParam(1)
	if err != nil {
		return err
	}

	setParam(2, op(p0, p1))
	ip += 4

	return nil
}

func jmpInst(cond func(int) bool) error {
	p0, err := getParam(0)
	if err != nil {
		return err
	}

	if cond(p0) {
		p1, err := getParam(1)
		if err != nil {
			return err
		}

		ip = p1
	} else {
		ip += 3
	}

	return nil
}

func run(getInput func() (int, error), setOutput func(int) error) error {
	for {
		oc := getMem(ip) % 100

		switch oc {
		case 1: // add
			err := binInst(func (p0, p1 int) int { return p0 + p1 })
			if err != nil {
				return err
			}

		case 2: // mul
			err := binInst(func (p0, p1 int) int { return p0 * p1 })
			if err != nil {
				return err
			}

		case 3: // in
			value, err := getInput()
			if err != nil {
				return err
			}

			err = setParam(0, value)
			if err != nil {
				return err
			}
			ip += 2

		case 4: // out
			value, err := getParam(0)
			if err != nil {
				return err
			}
			err = setOutput(value)
			ip += 2

		case 5: // jnz
			err := jmpInst(func (p0 int) bool { return p0 != 0 })
			if err != nil {
				return err
			}

		case 6: // jz
			err := jmpInst(func (p0 int) bool { return p0 == 0 })
			if err != nil {
				return err
			}

		case 7: // lt
			op := func (p0, p1 int) int {
				if p0 < p1 {
					return 1
				}
				return 0
			}
			err := binInst(op)
			if err != nil {
				return err
			}

		case 8: // eq
			op := func (p0, p1 int) int {
				if p0 == p1 {
					return 1
				}
				return 0
			}
			err := binInst(op)
			if err != nil {
				return err
			}

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

func getInput() (int, error) {
	data := make([]byte, 1)
	count, err := os.Stdin.Read(data)
	if count > 0 {
		return int(data[0]), nil
	}
	return 0, err
}

func setOutput(value int) error {
	fmt.Printf("%c", rune(value))
	return nil
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

	err = run(getInput, setOutput)
	if err != nil {
		log.Fatalf("%v\n", err)
	}
}
