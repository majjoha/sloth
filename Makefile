all: main

main:
	cd g_machine && make
	cd sestoft_machine && make

clean:
	cd g_machine && make clean
	cd sestoft_machine && make clean
