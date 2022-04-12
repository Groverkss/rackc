
.PHONY: all test 

all: runtime.o fake_prog
# test

test: runtime.o
	racket run-tests.rkt

runtime.o: runtime.c runtime.h
	gcc -std=c11 -c $^

fake_prog: fake_prog.c runtime.o
	gcc -std=c11 $^ -o $@

clean:
	rm -rf *~ fake_prog runtime.o runtime.h.gch ./compiled tests/*.s tests/*.out tests/*.dSYM tests/*~ *.dot *.png log.* *.log
