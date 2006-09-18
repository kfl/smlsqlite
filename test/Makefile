# smlsqlite - a binding of SQLite for Standard ML.
#
# Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.
# All rights reserved. See the file LICENSE for license information.

TESTS = treetest foldttest

all: test

# Compile
%: %.mlb
	mlton -default-ann 'allowFFI true' -link-opt "-lsqlite3" $<

treetest: $(shell mlton -stop f treetest.mlb)
foldttest: $(shell mlton -stop f foldttest.mlb)

# Run tests
.PHONY: test
test: $(TESTS)
	for t in $(TESTS); do ./$$t; done

# Clean up
.PHONY: clean
clean:
	$(RM) *~
	$(RM) $(TESTS)
	$(RM) $(patsubst %, %.db, $(TESTS))
