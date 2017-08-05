run: compile
	scala -classpath bin runtime.Application

compile: clean
	mkdir bin
	scalac -d bin src/*/*.scala

clean:
	rm -rf bin

