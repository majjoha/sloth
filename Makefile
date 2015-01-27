all: main

main: utils.o list.o
	gcc -o utils utils.c list.o

clean:
	rm -rf *o utils
