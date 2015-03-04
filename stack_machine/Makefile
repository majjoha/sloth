all: main

main:
	${CC} -m32 -g -c -o utils.o utils.c
	${CC} -m32 -g -o stack_machine utils.o stack_machine.c

clean:
	rm stack_machine
	rm *.o
	rm -rf *.dSYM
