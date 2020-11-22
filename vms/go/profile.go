// +build profile

package main

import (
	"fmt"
	"os"
)

var maxMemSize = 0
var minValue = 0
var maxValue = 0
var instCount = 0

func profileAddr(addr int) {
    if addr >= maxMemSize {
        maxMemSize = addr + 1
    }
}

func profileValue(value int) {
    if value > maxValue {
        maxValue = value
    }
    if value < minValue {
        minValue = value
    }
}

func profileInst() {
    instCount++
}

func profileDone() error {
    fmt.Fprintf(os.Stderr, "profile: maxMemSize = %d instCount = %d minValue = %d maxValue = %d\n",
        maxMemSize, instCount, minValue, maxValue);

    f, err := os.OpenFile("icvm_profile.csv", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
    if err != nil {
        return err
    }
    defer f.Close()

    _, err = fmt.Fprintf(f, "%d, %d, %d, %d\n", maxMemSize, instCount, minValue, maxValue)
    if err != nil {
        return err
    }

    return nil
}
