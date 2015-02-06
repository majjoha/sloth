all: main

main: ti.o list.o heap.o stack.o ti_node.o
	gcc -o ti -g ti.c list.o heap.o stack.o ti_node.o

clean:
	rm -rf *o ti *.dSYM
