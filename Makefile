all:test

RUNTIMEOBJ=runtime.c
clean:
	rm -f stst *.o *.s *.out test

scheme:
	gcc -O1 -o stst start.c stst.s

test:$(RUNTIMEOBJ) test_gc.c
	clang -g -o $@ $^

ctest:ctest.c
	clang -S ctest.c
	cat ctest.s
