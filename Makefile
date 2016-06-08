#SHELL=bash

all:
	cp EBNF/CX.cf .
	bnfc -haskell CX.cf
	happy -gca ParCX.y
	alex -g LexCX.x
	cabal build
	cp dist/build/CX/CX interpreter

test_good: all
	./interpreter good/testProg.cx
	./interpreter good/testBuiltinFunctions.cx
	./interpreter good/testVariableShadowing.cx
	./interpreter good/recursiveFunction.cx
	./interpreter good/operators.cx

test_bad: all
	- ./interpreter bad/internalFunctionArg.cx
	- ./interpreter bad/incorrectFunctionArgs.cx

clean:
	rm -rf *.{cf,x,y}
	rm -rf {Abs,Doc,Lex,Par,Print,Skel,Test}CX.*
	rm -rf ErrM.hs
	rm -f interpreter
