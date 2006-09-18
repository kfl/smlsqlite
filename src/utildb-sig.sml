(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

signature UTIL_DB = sig

    type db = Sqlite.db
    type status = Sqlite.status
    type sql = string

    val withDb   : string -> (db -> 'a) -> 'a
    val foldDb   : db -> (string list * 'a -> 'a) -> 'a -> sql -> 'a
    val modifyDb : db -> sql -> status

    type blob = Word8Vector.vector
    datatype value = Integer of int
                   | Float   of real
                   | Text    of string
                   | Blob    of blob
                   | Null

    val foldVal : db -> (value list * 'a -> 'a) -> 'a -> sql -> 'a

    type ('a, 'b) trans
    type ('cell, 'rest) cell_reader = ('cell -> 'rest, 'rest) trans
    val text   : (string, 'rest) cell_reader
    val string : (string, 'rest) cell_reader
    val int    : (int, 'rest)    cell_reader
    val real   : (real, 'rest)   cell_reader
    val blob   : (blob, 'rest)   cell_reader
    val value  : (value, 'rest)  cell_reader
    val option : ('a, 'rest) cell_reader -> ('a option, 'rest) cell_reader
    val --> : ('a, 'b) cell_reader * ('b, 'c) trans -> ('a -> 'b, 'c) trans 

    val foldT : db -> ('cols, 'acc -> 'acc) trans -> sql -> 'cols -> 'acc -> 'acc
end (* signature UTIL_DB *)

(*

   [withDb file f] opens the database named [file], calls the function
   [f] on the database handle, and closes the database.

   [foldDb db f b sql] executes the SQL statement [sql] and folds [f]
   over the result, where [b] is the base. The first argument to [f] is
   the values of the selected columns. Raises SQLiteError on error.

   [modifyDb db sql] modifies the database [db] by performing the
   SQL statements in [sql]. It is understood that [sql] does not
   contain queries, but this is not checked. The return value is
   the status code of executing the SQL statements.

   [value] is the type of result values with associated (dynamic) type
   information.  Note that SQLite allows the values in a column to
   have different types than the declared table schema.

   [foldVal db f b sql] executes the SQL statement [sql] and folds
   [f] over the result, where [b] is the base. The first argument to
   [f] is the values, of type value, of the selected columns with
   associated (dynamic) type information. Raises SQLiteError on error.

   [foldT db schema sql f b] executes the SQL statement [sql] and folds
   [f] over the result, where [b] is the base.  The result of [sql]
   must have a type compatable with [schema].  Raises SQLiteError on error.

*)
