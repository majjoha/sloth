all: main

main: ti.o list.o heap.o stack.o
	gcc -o ti -g ti.c list.o heap.o stack.o

clean:
	rm -rf *o ti
