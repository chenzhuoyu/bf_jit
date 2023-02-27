TARGET	= bfi
OBJECTS	= bfi.o

CFLAGS	= -O3 -Wall -fPIC
LDFLAGS	=

.PHONY: all clean

all: ${TARGET}

clean:
	rm -vrf ${TARGET} ${OBJECTS}

${TARGET}: ${OBJECTS}
	clang ${LDFLAGS} -o ${TARGET} ${OBJECTS}

%.o: %.c
	clang ${CFLAGS} -c $< -o $@
