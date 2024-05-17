CCriscv = riscv64-linux-gnu-gcc
CC = gcc
MAIN = runtime/main.c
ASM = main.s runtime.s
RUNTIME = runtime/runtime.c
clean:
	rm -f runtime/main.c a.out $(ASM)
build: clean
	cabal build
run: build
	cabal exec mlcps

asmc: run
	$(CC) -S $(MAIN) $(RUNTIME)
buildc: asmc
	$(CC) -g $(ASM)
runc: buildc
	./a.out

asmc-riscv: run 
	$(CCriscv) -S $(MAIN) $(RUNTIME)
buildc-riscv: asmc-riscv
	$(CCriscv) -g -static $(ASM)
runc-riscv: buildc-riscv
	qemu-riscv64 ./a.out
