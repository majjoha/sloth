all: main

shared = ../../shared
mm = ${shared}/mm

main:
	${CC} -m32 -std=c99 -g -c -o linked_list.o linked_list.c
	${CC} -m32 -std=c99 -g -c -o ${shared}/utils.o ${shared}/utils.c
	${CC} -m32 -std=c99 -g -c -o debug.o debug.c
	${CC} -m32 -std=c99 -g -c -o ${shared}/read_file.o ${shared}/read_file.c
	${CC} -m32 -std=c99 -g -c -o memory.o memory.c
	${CC} -m32 -std=c99 -g -o stack_machine debug.o ${shared}/read_file.o \
		linked_list.o memory.o ${shared}/utils.o stack_machine.c

clean:
	rm -f stack_machine
	rm -f *.o
	rm -f -rf *.dSYM
