// Ex: https://go.dev/tour/methods/23
package main

import (
	"io"
	"os"
	"strings"
)

type rot13Reader struct {
	r io.Reader
}

func rot13(c byte) byte {
	const Span = byte('z') - byte('a') + 1
	if c >= byte('A') && c <= byte('Z') {
		return (c - byte('A') + 13) % Span + byte('A')
	} else if c >= byte('a') && c <= byte('z') {
		return (c - byte('a') + 13) % Span + byte('a')
	} else {
		return c
	}
}

func (rot rot13Reader) Read(bytes []byte) (int, error) {
	n, err := rot.r.Read(bytes)
	if err != nil {
		return 0, err
	}
	for i := 0; i < n; i++ {
		bytes[i] = rot13(bytes[i])
	}
	return n, nil
}

func main() {
	s := strings.NewReader("Lbh penpxrq gur pbqr!")
	r := rot13Reader{s}
	io.Copy(os.Stdout, &r)
}
