(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

structure Sqlite :> SQLITE = struct

    open Dynlib
    local 
        val path = "smlsqlite.so"
        val hdl  = dlopen {lib = path, flag = RTLD_LAZY, global = false}
    in
        val symb = dlsym hdl
    end

    (* callbacks - mostly taken from mGTK *)

    type callback_data = string option list * string list
    type callback = callback_data -> bool
    type callback_id  = int

    val callbackTable : (int, callback) Polyhash.hash_table
        = Polyhash.mkPolyTable(401, Domain)
    val add     = Polyhash.insert callbackTable
    val peek    = Polyhash.peek callbackTable
    val destroy = Polyhash.remove callbackTable

    local
        val intern = ref 0;
    in
    val localId = fn f => f (!intern before intern := !intern + 1)
    end

    fun dispatch id data =
        case peek id of
            SOME f => f data    
          | NONE   => raise Fail("Sqlite: Unknown callback function (id: "^
				 Int.toString id^")")

    val _ = 
        ( Callback.register "msqlite_callback_dispatch" dispatch
        ; Callback.register "msqlite_callback_destroy"  destroy
        )
    fun register f = localId(fn id => (add (id, f); id))

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

    fun statusToString status =
        case status of 
            OK => "OK"
          | ERROR => "ERROR"
          | INTERNAL => "INTERNAL"
          | PERM => "PERM"
          | ABORT => "ABORT"
          | BUSY => "BUSY"
          | LOCKED => "LOCKED" 
          | NOMEM => "NOMEM" 
          | READONLY => "READONLY" 
          | INTERRUPT => "INTERRUPT" 
          | IOERR => "IOERR" 
          | CORRUPT => "CORRUPT" 
          | NOTFOUND => "NOTFOUND" 
          | FULL => "FULL" 
          | CANTOPEN => "CANTOPEN" 
          | PROTOCOL => "PROTOCOL" 
          | EMPTY => "EMPTY" 
          | SCHEMA => "SCHEMA" 
          | TOOBIG => "TOOBIG" 
          | CONSTRAINT => "CONSTRAINT" 
          | MISMATCH => "MISMATCH" 
          | MISUSE => "MISUSE" 
          | NOLFS => "NOLFS" 
          | AUTH => "AUTH" 
	  | FORMAT => "FORMAT"
	  | RANGE => "RANGE"
	  | NOTADB => "NOTADB"
          | ROW => "ROW" 
          | DONE => "DONE"
          | UNKNOWN_CODE code => "UNKNOWN_CODE ("^Int.toString code^")" 

    prim_type db

    exception SQLiteError of status * string

    val errcode : db -> status = app1(symb"msqlite_errcode")
    val errmsg  : db -> string = app1(symb"msqlite_errmsg")
    val close_db : db -> unit   = app1(symb"msqlite_close")
    val open_db_ : string -> status * db = app1(symb"msqlite_open")

    fun open_db filename = 
        case open_db_ filename of
            (OK,     db) => db
          | (status, db) => raise SQLiteError(status, 
                                              ("open_db: " ^ errmsg db)
                                              before close_db db) 


    val exec_ : db -> string -> int option -> status = app3(symb"msqlite_exec")

    (* FIXME: what if the callback raises an exception. *)
    fun exec db sql callback =
	let val id = Option.map register callback
	    val ret = exec_ db sql id
	    val _ = Option.app (ignore o destroy) id
	in  ret end
    fun exec' db sql callback = exec db sql (SOME callback)

    datatype column_type = INTEGER
                         | FLOAT
                         | TEXT
                         | BLOB
                         | NULL

    prim_type stmt
    val prepare_ : db -> string -> stmt * string option 
      = app2(symb"msqlite_prepare")
    fun prepare db sql =
	let val (stmt, _) = prepare_ db sql
	    val status = errcode db
	in  if status = OK then stmt
	    else raise SQLiteError(status, errmsg db)
	end
    val finalize : stmt -> status = app1(symb"msqlite_finalize")
    val reset : stmt -> status = app1(symb"msqlite_reset")
    val step : stmt -> status = app1(symb"msqlite_step")
    val data_count   : stmt -> int = app1(symb"msqlite_data_count")
    val column_count : stmt -> int = app1(symb"msqlite_column_count")
    val column_text  : stmt -> int -> string
      = app2(symb"msqlite_column_text")
    val column_name  : stmt -> int -> string = app2(symb"msqlite_column_name")
    val column_decltype : stmt -> int -> string option
      = app2(symb"msqlite_column_decltype")
    val column_type : stmt -> int -> column_type
      = app2(symb"msqlite_column_type")
    val column_double : stmt -> int -> real
      = app2(symb"msqlite_column_double")
    val column_int : stmt -> int -> int
      = app2(symb"msqlite_column_int")
    val column_real = column_double
    val column_blob : stmt -> int -> Word8Vector.vector
      = app2(symb"msqlite_column_blob")
end (* structure Sqlite *)
