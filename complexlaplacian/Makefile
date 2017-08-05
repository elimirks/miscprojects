SRC = src
LIB = -lsfml-system -lsfml-window -lsfml-graphics
OPTS = ${LIB} --std=gnu++11
SRCS = ${SRC}/complex.cpp ${SRC}/colorManipulationTools.cpp
BIN = complex

compile:
	g++ ${OPTS} ${SRCS} -o ${BIN}

run: compile
	./${BIN}

clean:
	rm ${BIN}
