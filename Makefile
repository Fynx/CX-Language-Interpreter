#SHELL=bash

all:
	cp EBNF/CX.cf .
	bnfc -haskell CX.cf
	happy -gca ParCX.y
	alex -g LexCX.x
	cabal build
	cp dist/build/CX/CX interpreter

test: all
	./interpreter test/testProg.cx
	./interpreter test/testBuiltinFunctions.cx
	./interpreter test/testVariableShadowing.cx
	./interpreter test/recursiveFunction.cx
	- ./interpreter test/bad/internalFunctionArg.cx
	- ./interpreter test/bad/incorrectFunctionArgs.cx

clean:
	rm -rf *.{cf,x,y}
	rm -rf {Abs,Doc,Lex,Par,Print,Skel,Test}CX.*
	rm -rf ErrM.hs
	rm -f interpreter
