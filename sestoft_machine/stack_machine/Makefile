all: main

shared = ../../shared
mm = ${shared}/mm

main:
	${CC} -m32 -g -c -o ${mm}/memory.o ${mm}/memory.c
	${CC} -m32 -g -c -o ${shared}/read_file.o ${shared}/read_file.c
	${CC} -m32 -g -o stack_machine ${mm}/memory.o ${shared}/read_file.o \
		stack_machine.c

clean:
	rm -f stack_machine
	rm -rf *.dSYM
