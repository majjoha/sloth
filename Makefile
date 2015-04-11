all: main

.PHONY: all main clean examples

main:
	cd g_machine && make
	cd sestoft_machine && make

clean:
	cd g_machine && make clean
	cd sestoft_machine && make clean

test:
	cd tests && ./run_tests.sh

examples:
	find examples -name "*.sl" | xargs -n 1 ./g_machine/compiler/slothc
	find examples -name "*.sl" | xargs -n 1 ./sestoft_machine/compiler/slothc
