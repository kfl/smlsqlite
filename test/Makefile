# smlsqlite - a binding of SQLite for Standard ML.
#
# Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.
# All rights reserved. See the file LICENSE for license information.

MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

SRC=../src
MOSMLSQLITE=../src/sqlite-sig.uo ../src/sqlite-mosml.uo ../src/utildb-sig.uo ../src/utildb.uo

TESTS = treetest foldttest

all: test

# Compilation for Moscow ML
%.ui %.uo: 
	$(MOSMLBIN)/mosmlc -c -toplevel $^

# Dependencies
foldttest.ui foldttest.uo: $(SRC)/utildb.ui foldttest.sml

foldttest: $(MOSMLSQLITE) foldttest.uo
	$(MOSMLBIN)/mosmlc -o $@ $^

# Compilation for MLton

%: %.mlb
	mlton -default-ann 'allowFFI true' -link-opt "-lsqlite3" $<

treetest-mlton: $(shell mlton -stop f treetest.mlb)
foldttest-mlton: $(shell mlton -stop f foldttest.mlb)

# Run tests
.PHONY: test
test: $(TESTS)
	for t in $(TESTS); do ./$$t; done

# Clean up
.PHONY: clean
clean:
	$(RM) *~ *.ui *.uo
	$(RM) $(TESTS)
	$(RM) $(patsubst %, %.db, $(TESTS))
