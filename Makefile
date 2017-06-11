#SHELL=bash

all:
	cp EBNF/CX.cf .
	bnfc -haskell CX.cf
	happy -gca ParCX.y
	alex -g LexCX.x
	cabal build
	cp dist/build/CX/CX interpreter

test_good: all
	./interpreter good/builtinFunctions.cx
	./interpreter good/variableShadowing.cx
	./interpreter good/recursiveFunction.cx
	./interpreter good/operators.cx
	./interpreter good/basicConstructs.cx
	./interpreter good/argFunction.cx

test_bad: all
	- ./interpreter bad/badForLoop.cx
	- ./interpreter bad/builtinFunctionArg.cx
	- ./interpreter bad/incorrectFunctionArgs.cx
	- ./interpreter bad/functionArgsNumber.cx
	- ./interpreter bad/zeroDivision.cx
	- ./interpreter bad/operatorArg.cx
	- ./interpreter bad/rvalue.cx
	- ./interpreter bad/returnMain.cx

clean:
	rm -rf *.{cf,x,y}
	rm -rf {Abs,Doc,Lex,Par,Print,Skel,Test}CX.*
	rm -rf ErrM.hs
	rm -f interpreter
