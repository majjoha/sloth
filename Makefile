all: main

.PHONY: all main clean examples

main:
	cd src/g_machine && make
	cd src/sestoft_machine && make

clean:
	cd src/g_machine && make clean
	cd src/sestoft_machine && make clean

test:
	cd tests && ./run_tests.sh

examples:
	find examples -name "*.sl" | xargs -n 1 ./src/g_machine/compiler/slothc
	find examples -name "*.sl" | xargs -n 1 ./src/sestoft_machine/compiler/slothc
