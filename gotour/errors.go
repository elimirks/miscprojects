// Ex: https://go.dev/tour/methods/20
package main

import (
	"fmt"
    "math"
)

type ErrNegativeSqrt float64

func Sqrt(x float64) (float64, error) {
    if x < 0.0 {
        return 0.0, ErrNegativeSqrt(x)
    }

    z := x
    for math.Abs(z*z - x) > 0.0001 {
        fmt.Println("z: ", z)
        z -= (z*z - x) / (2*z)
    }
    return z, nil
}

func (e ErrNegativeSqrt) Error() string {
    return fmt.Sprintf("cannot Sqrt negative number: %v", e)
}

func main() {
	fmt.Println(Sqrt(2))
	fmt.Println(Sqrt(-2))
}
