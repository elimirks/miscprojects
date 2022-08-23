// Ex: https://go.dev/tour/moretypes/18
package main

import "golang.org/x/tour/pic"

func Pic(dx, dy int) [][]uint8 {
    result := make([][]uint8, dy)
    for y := 0; y < dy; y++ {
        row := make([]uint8, dx)
        for x, _ := range row {
            row[x] = uint8((x+y)/2)
        }
        result[y] = row
    }
    return result
}

func main() {
    pic.Show(Pic)
}

