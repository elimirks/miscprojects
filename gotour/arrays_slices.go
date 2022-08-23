package main

import "fmt"

func main() {
    a()
    b()
    c()
}

func a() {
    primes := [6]int{2, 3, 5, 7, 11, 13}

    var s []int = primes[1:4]
    // Allocates extra memory since this number of values won't fit in `primes`
    s1 := append(s, 42, 43, 44, 45, 46, 47, 48)
    s1[0] = 1234
    fmt.Println(s)
    fmt.Println(s1)
    fmt.Println(primes)
}

func b() {
    primes := [6]int{2, 3, 5, 7, 11, 13}

    var s []int = primes[1:4]
    // Reuses `primes` memory
    s1 := append(s, 42)
    s1[0] = 1234
    fmt.Println(s)
    fmt.Println(s1)
    fmt.Println(primes)
}

func c() {
	var s []int
	printSlice(s)

	// append works on nil slices.
	s = append(s, 0)
	printSlice(s)

	// The slice grows as needed.
	s = append(s, 1)
	printSlice(s)

	// We can add more than one element at a time.
	s = append(s, 2, 3, 4)
	printSlice(s)
	
	var t = make([]int, 0, 5)
	t = append(t, 1, 2, 3, 4, 5)
	printSlice(t)
	t = append(t, 6)
	printSlice(t)
}

func printSlice(s []int) {
	fmt.Printf("len=%d cap=%d %v\n", len(s), cap(s), s)
}

