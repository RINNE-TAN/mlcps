clean:
	rm -f runtime/main.c a.out
build: clean
	cabal build
run: build
	cabal exec mlcps
buildc: run
	gcc -g runtime/main.c runtime/runtime.c
runc: buildc
	./a.out