package main

import "fmt"

// List represents a singly-linked list that holds
// values of any type.
type List[T any] struct {
	next *List[T]
	val  T
}

func Cons[T any](head T, tail *List[T]) *List[T] {
	return &List[T]{
		next: tail,
		val: head,
	}
}

func Tail[T any](list *List[T]) *List[T] {
	if list == nil || list.next == nil {
		return nil
	}
	return list.next
}

func (list *List[T]) String() string {
	if list == nil {
		return ""
	}
	return fmt.Sprintf("%v,%v", list.val, list.next)
}

func main() {
	l := Cons(123, Cons[uint8](42, nil))
	fmt.Printf("%v\n", l)
}
