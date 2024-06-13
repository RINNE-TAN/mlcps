CC = riscv64-linux-gnu-gcc
MAIN = runtime/main.s
MAIN_OBJ = output/main.o
EXE = output/main
ASM = $(MAIN) runtime/runtime.s runtime/stack.s

clean:
	rm -f $(MAIN) $(MAIN_OBJ) $(EXE)
build: clean
	cabal build
gencode: build
	cabal exec mlcps
asm: gencode
	riscv64-linux-gnu-as -g -o $(MAIN_OBJ) $(ASM) 
ld: asm 
	$(CC) -g -o $(EXE) $(MAIN_OBJ) -static -lc
run: ld
	qemu-riscv64 ./$(EXE)
qemu: ld
	qemu-riscv64 -g 1234 $(EXE)
gdb:
	gdb-multiarch -x gdbinit main
