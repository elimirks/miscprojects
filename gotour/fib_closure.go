// Ex: https://go.dev/tour/moretypes/26
package main

import "fmt"

// fibonacci is a function that returns
// a function that returns an int.
func fibonacci() func() int {
    prev := 0
    curr := 0
    next := 1
    return func() int {
        prev = curr
        curr = next
        next = curr + prev
        return prev
    }
}

func main() {
    f := fibonacci()
    for i := 0; i < 10; i++ {
        fmt.Println(f())
    }
}

