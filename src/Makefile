# smlsqlite - a binding of SQLite for Standard ML.
#
# Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.
# All rights reserved. See the file LICENSE for license information.

MOSMLHOME=/usr/lib/mosml
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLBIN=/usr/bin

# Compilation for Moscow ML

smlsqlite.so: smlsqlite.o
	gcc -shared -o $@ -Wl,--export-dynamic -lsqlite3 $<

smlsqlite.o: smlsqlite.c
	gcc -c -O3 -fPIC -Wall -Dunix -I$(MOSMLRUNTIME) $<

%.ui %.uo: 
	$(MOSMLBIN)/mosmlc -c -toplevel $^

# Dependencies
sqlite-sig.ui sqlite-sig.uo: sqlite-sig.sml
sqlite-mosml.ui sqlite-mosml.uo: sqlite-sig.ui sqlite-mosml.sml
utildb-sig.ui utildb-sig.uo: sqlite-mosml.ui utildb-sig.sml
utildb.ui utildb.uo: sqlite-mosml.ui utildb-sig.ui utildb.sml

.PHONY: clean
clean:
	$(RM) *~ *.ui *.uo 
	$(RM) smlsqlite.o smlsqlite.so
