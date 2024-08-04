package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"runtime"
	"strconv"
	"strings"
)

var mem []int

var ip = 0
var rb = 0

func resizeMem(addr int) {
	if addr >= len(mem) {
		newSize := len(mem)
		for addr >= newSize {
			newSize <<= 1
		}
		newMem := make([]int, newSize)
		copy(newMem, mem)
		mem = newMem
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

func getParams(cnt int) ([]int, error) {
	out := make([]int, cnt);

	for idx := 0; idx < cnt; idx++ {
		switch getMem(ip) / modeMul[idx] % 10 {
			case 0: // position mode
				out[idx] = getMem(getMem(ip + idx + 1))
			case 1: // immediate mode
				out[idx] = getMem(ip + idx + 1)
			case 2: // relative mode
				out[idx] = getMem(rb + getMem(ip+idx+1))
			default:
				return nil, fmt.Errorf("mode error: ip %d idx %d", ip, idx)
		}
	}

	return out, nil
}

func setParam(idx, val int) error {
	mode := getMem(ip) / modeMul[idx] % 10

	switch mode {
		case 0: // position mode
			setMem(getMem(ip+idx+1), val)
		case 2: // relative mode
			setMem(rb+getMem(ip+idx+1), val)
		default:
			return fmt.Errorf("mode error: ip %d idx %d", ip, idx)
	}

	return nil
}

func binInst(op func(int, int) int) error {
	p, err := getParams(2);
	if err != nil {
		return err
	}

	setParam(2, op(p[0], p[1]))
	ip += 4

	return nil
}

func jmpInst(cond func(int) bool) error {
	p, err := getParams(2)
	if err != nil {
		return err
	}

	if cond(p[0]) {
		ip = p[1]
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
			err := binInst(func(p0, p1 int) int { return p0 + p1 })
			if err != nil {
				return err
			}

		case 2: // mul
			err := binInst(func(p0, p1 int) int { return p0 * p1 })
			if err != nil {
				return err
			}

		case 3: // in
			value, err := getInput()
			if (err == io.EOF) {
				endl := "\n"
				if (runtime.GOOS == "windows") {
					endl = "\r\n"
				}
				os.Stderr.WriteString("no more inputs" + endl)
				os.Exit(1)
			}
			if err != nil {
				return err
			}

			err = setParam(0, value)
			if err != nil {
				return err
			}
			ip += 2

		case 4: // out
			p, err := getParams(1)
			if err != nil {
				return err
			}
			err = setOutput(p[0])
			if err != nil {
				return err
			}
			ip += 2

		case 5: // jnz
			err := jmpInst(func(p0 int) bool { return p0 != 0 })
			if err != nil {
				return err
			}

		case 6: // jz
			err := jmpInst(func(p0 int) bool { return p0 == 0 })
			if err != nil {
				return err
			}

		case 7: // lt
			err := binInst(func(p0, p1 int) int { if p0 < p1 { return 1 }; return 0 })
			if err != nil {
				return err
			}

		case 8: // eq
			err := binInst(func(p0, p1 int) int { if p0 == p1 { return 1 }; return 0 })
			if err != nil {
				return err
			}

		case 9: // arb
			p, err := getParams(1)
			if err != nil {
				return err
			}
			rb += p[0]
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

	f, err := os.ReadFile(os.Args[1])
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
		log.Fatalf("xxx %v\n", err)
	}
}
