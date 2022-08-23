// https://go.dev/tour/concurrency/10

package main

import (
	"fmt"
	"sync"
    "time"
)

type Fetcher interface {
	// Fetch returns the body of URL and
	// a slice of URLs found on that page.
	Fetch(url string) (body string, urls []string, err error)
	// Decides if the Crawl function should launch a crawler for the given URL
	ShouldFetch(url string) bool
}

// Crawl uses fetcher to recursively crawl
// pages starting with url, to a maximum of depth.
func Crawl(url string, depth int, fetcher Fetcher) {
	if depth <= 0 {
		return
	}
	if !fetcher.ShouldFetch(url) {
		return
	}
	body, urls, err := fetcher.Fetch(url)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("found: %s %q\n", url, body)
	for _, u := range urls {
		go Crawl(u, depth-1, fetcher)
	}
	return
}

func main() {
	Crawl("https://golang.org/", 4, fetcher)
	// Wait for parallelized goroutines to complete
	// Sloppy way to wait for completion, but fine enough for the exercise
	time.Sleep(1 * time.Second)
}

// fakeFetcher is Fetcher that returns canned results.
type fakeFetcher struct {
	results map[string]*fakeResult
	// Visited set of URLs
	visited map[string]bool
	mutex   sync.Mutex
}

type fakeResult struct {
	body string
	urls []string
}

func (f fakeFetcher) Fetch(url string) (string, []string, error) {
	if res, ok := f.results[url]; ok {
		return res.body, res.urls, nil
	}
	return "", nil, fmt.Errorf("not found: %s", url)
}

func (f fakeFetcher) ShouldFetch(url string) bool {
	f.mutex.Lock()
	defer f.mutex.Unlock()
	if f.visited[url] {
		return false
	}
	f.visited[url] = true
	return true
}

var fakeResults = map[string]*fakeResult{
	"https://golang.org/": &fakeResult{
		"The Go Programming Language",
		[]string{
			"https://golang.org/pkg/",
			"https://golang.org/cmd/",
		},
	},
	"https://golang.org/pkg/": &fakeResult{
		"Packages",
		[]string{
			"https://golang.org/",
			"https://golang.org/cmd/",
			"https://golang.org/pkg/fmt/",
			"https://golang.org/pkg/os/",
		},
	},
	"https://golang.org/pkg/fmt/": &fakeResult{
		"Package fmt",
		[]string{
			"https://golang.org/",
			"https://golang.org/pkg/",
		},
	},
	"https://golang.org/pkg/os/": &fakeResult{
		"Package os",
		[]string{
			"https://golang.org/",
			"https://golang.org/pkg/",
		},
	},
}

// fetcher is a populated fakeFetcher.
var fetcher = fakeFetcher{
	results: fakeResults,
	visited: make(map[string]bool),
}
