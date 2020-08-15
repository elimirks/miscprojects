# Compiler
CC   = gcc
OPTS = -std=gnu11 -pedantic -g -O3

# Project name
PROJECT = lcvm

SRC_DIR = 'src'
OBJ_DIR = 'obj'

SRCS = obj/lcvm.tab.c src/main.c src/vm.c

# Targets
$(PROJECT): buildrepo compileParser
	$(CC) $(OPTS) $(SRCS) -o $@

clean:
	rm $(PROJECT) $(OBJ_DIR) -Rf

compileParser:
	lex -o $(OBJ_DIR)/lcvm.yy.c $(SRC_DIR)/lcvm.l
	bison -o $(OBJ_DIR)/lcvm.tab.c $(SRC_DIR)/lcvm.y

buildrepo:
	mkdir -p $(OBJ_DIR)
