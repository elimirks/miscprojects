SRC = src
LIB =
OPTS = ${LIB} --std=c17 -g
SRCS = ${SRC}/main.c
BIN = dynamic_comp

compile:
	gcc ${OPTS} ${SRCS} -o ${BIN}

run: compile
	./${BIN}

clean:
	rm ${BIN}
