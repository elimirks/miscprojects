# Compiler
CC   = clang++
OPTS = -g -O3 `llvm-config --cxxflags`

# Project name
PROJECT = kaleidoscope

SRC_DIR = 'src'
OBJ_DIR = 'obj'

SRCS = src/main.c

# Targets
$(PROJECT): buildrepo
	$(CC) $(SRCS) -o $@

clean:
	rm $(PROJECT) $(OBJ_DIR) -Rf

buildrepo:
	mkdir -p $(OBJ_DIR)

