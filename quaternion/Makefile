SRC = src
LIB = -lsfml-graphics -lsfml-window -lsfml-system
OPTS = -g --std=gnu++11 ${LIB}
BIN = quaternion

OBJECTS = $(patsubst %.cpp, %.o, $(wildcard src/*.cpp))

$(BIN): $(OBJECTS)
	g++ -o $@ $^ ${OPTS}

${OBJ}/%.o: ${SRC}/%.cpp
	g++ $^ -o $@ ${OPTS} 

clean:
	rm -rf ${BIN} ${OBJECTS}
