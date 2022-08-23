package main

import "golang.org/x/tour/reader"

type MyReader struct{}

func (_ MyReader) Read(bytes []byte) (int, error) {
    bytes[0] = byte('A')
    return 1, nil
}

func main() {
	reader.Validate(MyReader{})
}

