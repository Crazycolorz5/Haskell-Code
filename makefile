AssemblyQuicksort:AssemblyQuicksort.o
	ghc -O2 ForeignQuicksort.hs AssemblyQuicksort.o -o AssemblyQuicksort
	make clean
AssemblyQuicksort.o:
	g++ -c AssemblyQuicksort.cpp -o AssemblyQuicksort.o
ForeignQuicksort: AssemblyQuicksort.o
	ghc -O2 ForeignQuicksort.hs AssemblyQuicksort.o -o ForeignQuicksort
	make clean

ImperativeQuicksort:
	ghc -O2 ImperativeQuicksort.hs -o ImperativeQuicksort
	make clean

tools:
	ghc --make -O2 -c -o MyUsefulFunctions.o MyUsefulFunctions.hs

clean:
	rm -f *.hi *.exe

