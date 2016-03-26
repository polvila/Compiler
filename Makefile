all:
	bison -d -v bis.y
	flex lex.l
	gcc lex.yy.c bis.tab.c functions.c vector.c symtab.c -o executable.o -ll -lm
	
run:
	./executable.o testCalcOn.c
	./executable.o testCalcOff.c
clean:
	rm bis.output
	rm bis.tab.c
	rm bis.tab.h
	rm executable.o
	rm lex.yy.c