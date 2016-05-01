all:
	cp EBNF/CX.cf .
	bnfc -haskell CX.cf
	happy -gca ParCX.y
	alex -g LexCX.x
	cabal build -v

clean:
	rm -rf *.{cf,x,y}
	rm -rf {Abs,Doc,Lex,Par,Print,Skel,Test}CX.*
	rm -rf ErrM.hs
