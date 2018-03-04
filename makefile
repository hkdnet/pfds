all: \
	bin/9-2 \
	bin/9-4

bin/9-2: 9-2.hs
	ghc -Wall -o $@ $^
bin/9-4: 9-4.hs
	ghc -Wall -o $@ $^

