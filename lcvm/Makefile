# Compiler
CC   = gcc
OPTS = -std=gnu11 -pedantic -g -O3

# Project name
PROJECT = lcvm

SRC_DIR = 'src'
OBJ_DIR = 'obj'

SRCS = src/main.c src/scanner.c

# Targets
$(PROJECT): buildrepo compileScanner
	$(CC) $(OPTS) $(SRCS) -o $@

clean:
	rm $(PROJECT) $(OBJ_DIR) -Rf

compileScanner:
	lex -o $(OBJ_DIR)/lcvm.yy.c $(SRC_DIR)/lcvm.lex

buildrepo:
	mkdir -p $(OBJ_DIR)
