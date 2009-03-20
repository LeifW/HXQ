# the database driver must be mysql, sqlite, or nodb
driver = mysql

ifeq (${driver},mysql)
args = -isrc -isrc/hxml-0.2 -isrc/withDB -isrc/mysql
else ifeq (${driver},sqlite)
args = -isrc -isrc/hxml-0.2 -isrc/withDB -isrc/sqlite
else
args = -isrc -isrc/hxml-0.2 -isrc/noDB
endif

parser = src/Text/XML/HXQ/Parser.hs
ghc = ghc -O2 -funfolding-use-threshold=16 ${args}
src = * src/hxml-0.2/* src/noDB/Text/XML/HXQ/* src/withDB/Text/XML/HXQ/* src/* src/Text/XML/HXQ/* src/mysql/* src/sqlite/*

# build the xquery interpreter
all:    $(parser) Main.hs
	$(ghc) --make Main.hs -o xquery

# generate the XQuery parser using happy
$(parser): XQueryParser.y
	happy -g -a -c -o $(parser) XQueryParser.y

test1:  $(parser) Test1.hs
	$(ghc) --make Test1.hs -o a.out
	./a.out

test2:  $(parser) Test2.hs
	$(ghc) --make Test2.hs -o a.out
	time ./a.out +RTS -H2m -M3.2m

# uses quasi-quotes (for ghc >= 6.9 only)
test2a: $(parser) Test2a.hs
	$(ghc) --make Test2a.hs -o a.out
	time ./a.out +RTS -H2m -M3.2m

test3:  $(parser) TestDB.hs
	$(ghc) --make TestDB.hs -o a.out
	./a.out

test4:  $(parser) TestDB2.hs
	$(ghc) --make TestDB2.hs -o a.out
	./a.out

# extract the structural summary of dblp.xml
dblp1:  $(parser) TestDBLP1.hs
	$(ghc) --make TestDBLP1.hs -o a.out
	time ./a.out +RTS -H200m -K100m

# store dblp.xml into a MySQL database
dblp2:  $(parser) TestDBLP2.hs
	$(ghc) --make TestDBLP2.hs -o a.out
	time ./a.out

# run in the ghci interpreter and load HXQ
ghci:   $(parser)
	ghci -fth ${args} Main.hs

# run multiple tests
test:   $(parser) data/test.xq
	./xquery data/test.xq | diff - data/test-results.txt

# heap profiling
profile: $(parser) Test2.hs
	ghc -O2 -funfolding-use-threshold=16 -isrc -isrc/hxml-0.2 -isrc/noDB --make Test2.hs -o a.out
	ghc -O2 -funfolding-use-threshold=16 -isrc -isrc/hxml-0.2 -isrc/noDB -prof -auto-all -osuf p_o --make Test2.hs -o a.out
	time ./a.out +RTS -hc
	hp2ps -color a.out.hp

# create the cabal distribution
cabal:	$(parser)
	runhaskell Setup.lhs configure
	runhaskell Setup.lhs build
	runhaskell Setup.lhs haddock
	runhaskell Setup.lhs sdist

clean:
	/bin/rm -f xquery Temp.hs a.out $(addsuffix .hi,$(src)) $(addsuffix .o,$(src)) $(addsuffix .p_o,$(src))

distclean: clean
	runhaskell Setup.lhs clean
