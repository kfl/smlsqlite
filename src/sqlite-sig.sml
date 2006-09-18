(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

signature SQLITE = sig

    type db

    datatype status = OK
                    | ERROR
                    | INTERNAL
                    | PERM
                    | ABORT
                    | BUSY
                    | LOCKED 
                    | NOMEM 
                    | READONLY 
                    | INTERRUPT 
                    | IOERR 
                    | CORRUPT 
                    | NOTFOUND 
                    | FULL 
                    | CANTOPEN 
                    | PROTOCOL 
                    | EMPTY 
                    | SCHEMA 
                    | TOOBIG 
                    | CONSTRAINT 
                    | MISMATCH 
                    | MISUSE 
                    | NOLFS 
                    | AUTH
                    | FORMAT
                    | RANGE
                    | NOTADB 
                    | ROW 
                    | DONE
                    | UNKNOWN_CODE of int
                      
    val statusToString : status -> string
    exception SQLiteError of status * string

    val open_db  : string -> db 
    val close_db : db -> unit
    val errcode  : db -> status
    val errmsg   : db -> string

(*    type callback = string option list * string list -> bool
    val exec  : db -> string -> callback option -> status
    val exec' : db -> string -> callback        -> status
*)
    type stmt
    val prepare : db -> string -> stmt
    val finalize : stmt -> status
    val step : stmt -> status
    val reset : stmt -> status

    datatype column_type = INTEGER
                         | FLOAT
                         | TEXT
                         | BLOB
                         | NULL

    val column_count : stmt -> int
    val column_name : stmt -> int -> string
    val column_decltype : stmt -> int -> string option
    val column_type : stmt -> int -> column_type
(*
    val data_count : stmt -> int
*)
    val column_text   : stmt -> int -> string
    val column_blob   : stmt -> int -> Word8Vector.vector
    val column_int    : stmt -> int -> int
    val column_double : stmt -> int -> real
    val column_real   : stmt -> int -> real

end (* signature SQLITE *)

(*

   [open_db file] opens the database named [file]. If the database
   does not exists a new database is created. 

   [close_db db] closes a previously opened database.

   [errcode db] returns the error code for the most recently failed
   SQLite call.

   [errmsg db] returns the error message for the most recently failed
   SQLite call.


   [stmt] is the type of compiled SQL statements.

   [prepare db sql] compiles the first SQL statement in [sql] for
   later execution by [step]. The functions returns the compiled
   statement.

   [finalize stmt] deletes the statement [stmt] and frees associated
   resources. All prepared statements must be finalized before closing
   the database.

   [step stmt] executes one step of the statement [stmt]. [step]
   returns [DONE] if the execution was successful. If [stmt] is a
   query, then for each step of the execution, [step] will return
   [ROW] and the result can be inspected using the [column_]
   functions. The complete set of rows of the result is obtained by
   repeatedly invoking [step].

   [reset stmt] resets the statement [step] so it can be re-executed.


   [column_type] is the type of column results.

   [column_count stmt] returns then number of columns in the result
   returned by the statement [stmt].

   [column_name stmt col] returns the column heading for column
   number [col].

   [column_decltype stmt col] if the SQL statement [stmt] is a SELECT
   statement and the type used when declaring column [col] of the
   table referenced by [stmt] was s, then return SOME s, otherwise
   return NONE.

   [column_type stmt col] return the data type of column [col] of the
   result.

   [column_text stmt col] return column [col] of the result as a string.

   [column_blob stmt col] return column [col] of the result as a blob.

   [column_int stmt col] return column [col] of the result as a integer.

   [column_double stmt col] return column [col] of the result as a real.

   [column_real stmt col] return column [col] of the result as a real.

*)
   
