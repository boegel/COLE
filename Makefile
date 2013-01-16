all: example example2 example3 examplePBS

%: GA.hs COLE.hs %.hs
	ghc --make -Wall $@

clean:
	rm -f *.hi *.o example example2 example3 examplePBS
