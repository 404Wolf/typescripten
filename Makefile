all: adder

adder.ll:
	cargo run > adder.ll

adder.o: adder.ll
	llc -filetype=obj -O0 adder.ll -o adder.o

adder: adder.o
	clang -O0 adder.o -o adder

clean:
	rm -f adder.ll adder.o adder

.PHONY: all clean
