// Ex: https://go.dev/tour/moretypes/23
package main

import (
    "golang.org/x/tour/wc"
    "strings"
)

func WordCount(s string) map[string]int {
    result := make(map[string]int)
    for _, word := range strings.Fields(s) {
        result[word] += 1
    }
    return result
}

func main() {
    wc.Test(WordCount)
}
