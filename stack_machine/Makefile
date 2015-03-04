all: main

main:
	${CC} -c -o utils.o utils.c
	${CC} -o stack_machine utils.o stack_machine.c

clean:
	rm stack_machine
	rm *.o
