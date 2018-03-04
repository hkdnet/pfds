all: \
	bin/9-2

bin/9-2: 9-2.hs
	ghc -Wall -o $@ $^

