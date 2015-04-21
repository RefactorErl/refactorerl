default:    tool

all:	tool doc bufsrv

tool:
	bin/referl -build tool

bufsrv:
	cabal install --bindir=bin

doc:
	bin/referl -build doc

clean:
	bin/referl -build clean

cleanall:
	bin/referl -build clean
	cabal clean

