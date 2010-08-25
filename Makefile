
RUNS ?= 10000
GHC = ghc --make -outputdir

test/echo: test/Echo.hs
	mkdir -p test/build
	${GHC} test/build -O2 test/Echo.hs -o test/echo

prof/echo: test/Echo.hs
	mkdir -p prof/build
	${GHC} prof/build test/Echo.hs -o prof/echo -prof -auto-all

clean:
	rm -rf */build

.PHONY: test
test: test/echo
	test/echo ${RUNS}

.PHONY: prof
prof: prof/echo
	cd prof && ./echo ${RUNS} +RTS -hb -RTS && hp2ps echo.hp

