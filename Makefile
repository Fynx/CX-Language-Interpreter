all:
	cp EBNF/CX.cf .
	bnfc -haskell CX.cf
	happy -gca ParCX.y
	alex -g LexCX.x
	cabal build -v
	cp dist/build/CX/CX interpreter

test: all
	./interpreter test/testProg.cx

clean:
	rm -rf *.{cf,x,y}
	rm -rf {Abs,Doc,Lex,Par,Print,Skel,Test}CX.*
	rm -rf ErrM.hs
