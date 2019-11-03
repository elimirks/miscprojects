SRC = src
LIB = -lsfml-graphics -lsfml-window -lsfml-system
OPTS = --std=gnu++11 ${LIB}
SRCS = ${SRC}/main.cpp
BIN = multigrid

OBJECTS = $(patsubst %.cpp, %.o, $(wildcard src/*.cpp))

$(BIN): $(OBJECTS)
	g++ -o $@ $^ ${OPTS}

${OBJ}/%.o: ${SRC}/%.cpp
	g++ ${OPTS} $^ -o $@

clean:
	rm -rf ${BIN} ${OBJECTS}
