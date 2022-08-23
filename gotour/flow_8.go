package main

import (
    "fmt"
    "math"
)

func Sqrt(x float64) float64 {
    z := x
    for math.Abs(z*z - x) > 0.0001 {
        fmt.Println("z: ", z)
        z -= (z*z - x) / (2*z)
    }
    return z
}

func main() {
    fmt.Println(Sqrt(2))
}
