# Compiler
CC   = clang++
LINKER_FLAGS = -O3 `llvm-config --ldflags --cxxflags --system-libs --libs core`
CPP_FLAGS = -g -O3 `llvm-config --cxxflags`

# Project name
PROJECT = kaleidoscope

SRC_FILES := $(wildcard src/*.cpp)
OBJ_FILES := $(patsubst src/%.cpp,obj/%.o,$(SRC_FILES))

all: obj $(PROJECT)

$(PROJECT): $(OBJ_FILES)
	$(CC) $(LINKER_FLAGS) -o $@ $^

obj/%.o: src/%.cpp
	$(CC) $(CPP_FLAGS) -c -o $@ $<

clean:
	rm $(PROJECT) obj -Rf

obj:
	mkdir -p obj
