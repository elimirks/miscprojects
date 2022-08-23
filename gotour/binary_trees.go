package main

import (
	"golang.org/x/tour/tree"
	"fmt"
	"math"
	"sort"
)

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	//walkp(t, ch)
	walkpClosure(t, ch)
	close(ch)
}

// Walks without closing the channel
func walkp(t *tree.Tree, ch chan int) {
	if t == nil {
		return
	}
	// A bit sloppy
	cur := t.Value
	lhs := math.MaxInt
	rhs := math.MaxInt

	if t.Left != nil {
		lhs = t.Left.Value
	}
	if t.Right != nil {
		rhs = t.Right.Value
	}
	// 6 possible permutations of {cur, lhs, rhs}
	if lhs < cur {
		if lhs < rhs {
			if rhs < cur {
				// lhs < rhs < cur
				walkp(t.Left, ch)
				walkp(t.Right, ch)
				ch <- t.Value
			} else {
				// lhs < cur < rhs
				walkp(t.Left, ch)
				ch <- t.Value
				walkp(t.Right, ch)
			}
		} else {
			// rhs < lhs < cur
			walkp(t.Right, ch)
			walkp(t.Left, ch)
			ch <- t.Value
		}
	} else {
		if rhs < cur {
			// rhs < cur < lhs
			walkp(t.Right, ch)
			ch <- t.Value
			walkp(t.Left, ch)
		} else {
			if lhs < rhs {
				// cur < lhs < rhs
				ch <- t.Value
				walkp(t.Left, ch)
				walkp(t.Right, ch)
			} else {
				// cur < rhs < lhs
				ch <- t.Value
				walkp(t.Right, ch)
				walkp(t.Left, ch)
			}
		}
	}
}

type WalkComputation struct {
	SortKey int
	Run func()
}

// A bit fancier technique, but probably slower
func walkpClosure(t *tree.Tree, ch chan int) {
	if t == nil {
		return
	}

	computations := make([]WalkComputation, 1, 3)
	computations[0] = WalkComputation {
		SortKey: t.Value,
		Run: func () {
			ch <- t.Value
		},
	}

	if t.Left != nil {
		computations = append(computations, WalkComputation {
			SortKey: t.Left.Value,
			Run: func () {
				walkpClosure(t.Left, ch)
			},
		})
	}
	if t.Right != nil {
		computations = append(computations, WalkComputation {
			SortKey: t.Right.Value,
			Run: func () {
				walkpClosure(t.Right, ch)
			},
		})
	}

	sort.Slice(computations, func(i, j int) bool {
        return computations[i].SortKey < computations[j].SortKey
    })

	for _, comp := range computations {
		comp.Run()
	}
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	c1 := make(chan int)
	c2 := make(chan int)

	go Walk(t1, c1)
	go Walk(t2, c2)

	for {
		v1, ok1 := (<-c1)
		v2, ok2 := (<-c2)
		if !ok1 {
			return ok1 == ok2
		}
		if v1 != v2 {
			return false
		}
	}
}

func main() {
	fmt.Println(Same(tree.New(1), tree.New(1)))
	fmt.Println(Same(tree.New(1), tree.New(2)))
}
