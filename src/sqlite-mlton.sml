(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

structure Sqlite :> SQLITE = 
struct
    structure F = MLton.Finalizable

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
          | RANGE  => "RANGE"
          | NOTADB => "NOTADB"
          | ROW => "ROW" 
          | DONE => "DONE"
          | UNKNOWN_CODE code => "UNKNOWN_CODE ("^Int.toString code^")" 
				    
    fun statusFromSqliteStatus status =
	case status of
	    0 => OK
	  | 1 => ERROR
	  | 2 => INTERNAL
	  | 3 => PERM
	  | 4 => ABORT
	  | 5 => BUSY
	  | 6 => LOCKED
	  | 7 => NOMEM
	  | 8 => READONLY
	  | 9 => INTERRUPT
	  | 10 => IOERR
	  | 11 => CORRUPT
	  | 12 => NOTFOUND
	  | 13 => FULL
	  | 14 => CANTOPEN
	  | 15 => PROTOCOL
	  | 16 => EMPTY
	  | 17 => SCHEMA
	  | 18 => TOOBIG
	  | 19 => CONSTRAINT
	  | 20 => MISMATCH
	  | 21 => MISUSE
	  | 22 => NOLFS
	  | 23 => AUTH
	  | 24 => FORMAT
	  | 25 => RANGE
	  | 26 => NOTADB
	  | 100 => ROW
	  | 101 => DONE
          | unknown => UNKNOWN_CODE unknown


    type db = MLton.Pointer.t
    type sqlite_status = int
    val null = MLton.Pointer.null

    exception SQLiteError of status * string

    val errcode_ = _import "sqlite3_errcode" : db -> sqlite_status; 
    val errcode = statusFromSqliteStatus o errcode_
    val errmsg_ = _import "sqlite3_errmsg" : db -> CString.t;
    fun errmsg db = CString.toStringVal(errmsg_ db)

    val close_db = _import "sqlite3_close" : db -> unit;
    val open_db_ = _import "sqlite3_open" : CString.cstring * db ref -> sqlite_status;
    (* FIXME: handle errors *)
    fun open_db filename = 
        let val cfilename = CString.fromString filename
            val dbr = ref null
        in  open_db_ (cfilename, dbr)
          ; !dbr
        end

    (*
     val exec_ : db -> string -> int option -> status = app3(symb"msqlite_exec")

    (* FIXME: what if the callback raises an exception. *)
    fun exec db sql callback =
	let val id = Option.map register callback
	    val ret = exec_ db sql id
	    val _ = Option.app (ignore o destroy) id
	in  ret end
    fun exec' db sql callback = exec db sql (SOME callback)
     *)

    datatype column_type = INTEGER
                         | FLOAT
                         | TEXT
                         | BLOB
                         | NULL


    type stmt_ptr = MLton.Pointer.t
    val finalize_ = _import "sqlite3_finalize" : stmt_ptr -> sqlite_status;
    type stmt = stmt_ptr ref F.t
    fun finalize_simple (stmt as ref ptr) =
        if ptr <> null then finalize_ ptr before stmt := null
        else 0 (* OK, as also returned by sqlite3_finalize *)
    fun makeStmt ptr = 
        let val result = F.new ptr
        in  F.addFinalizer(result, ignore o finalize_simple)
          ; result
        end

    fun stmtUnwrap f (ref ptr) = if ptr <> null then f ptr
                                 else raise Fail "Statement finalized"
    fun withStmt(stmt, f) = F.withValue(stmt, stmtUnwrap f)

    fun finalize stmt = 
	statusFromSqliteStatus(F.withValue(stmt, finalize_simple))
                        
    val prepare_ = _import "sqlite3_prepare" 
                 : db * CString.cstring * int * stmt_ptr ref * MLton.Pointer.t ref ->  int;
    fun prepare db sql =
        let val csql = CString.fromString sql
            val stmt_ptr = ref null
            val dummy = ref null
            val status = statusFromSqliteStatus(
                         prepare_(db, csql, CharVector.length sql, 
                                  stmt_ptr, dummy))
        in  if status = OK then makeStmt stmt_ptr
            else raise SQLiteError(status, errmsg db)
        end
  
    val reset_  = _import "sqlite3_reset" : stmt_ptr -> sqlite_status;
    fun reset stmt = statusFromSqliteStatus(withStmt(stmt, reset_))

    val step_ = _import "sqlite3_step" : stmt_ptr -> sqlite_status;
    fun step stmt = statusFromSqliteStatus(withStmt(stmt, step_))
 
    val data_count_ = _import "sqlite3_data_count" : stmt_ptr -> int;
    fun data_count stmt = withStmt(stmt, data_count_)

    val column_count_ = _import "sqlite3_column_count" : stmt_ptr -> int;
    fun column_count stmt = withStmt(stmt, column_count_)

    val column_text_ = 
        _import "sqlite3_column_text" : stmt_ptr * int -> CString.t;
    fun column_text stmt col =
        withStmt(stmt, fn ptr =>
                 let val t = column_text_(ptr, col)
                 in  CString.toStringVal t
                 end)

    val column_name_ = 
        _import "sqlite3_column_name" : stmt_ptr * int -> CString.t;
    fun column_name stmt col =
        withStmt(stmt, fn ptr =>
                 let val t = column_name_(ptr, col)
                 in  CString.toStringVal t
                 end)


    val column_decltype_ = 
        _import "sqlite3_column_decltype" : stmt_ptr * int -> CString.t;
    fun column_decltype stmt col =
        withStmt(stmt, fn ptr =>
                 let val t = column_decltype_(ptr, col)
                 in  CString.toString t
                 end)

    type raw_column_type = int
    fun columnTypeFromRaw ct =
        case ct of
            1 => INTEGER
          | 2 => FLOAT
          | 3 => TEXT
          | 4 => BLOB
          | 5 => NULL
          | _ => raise Fail ("Incompatability with installed SQLite. "^
                             "Unknown column_type: "^Int.toString ct)
    val column_type_ =
        _import "sqlite3_column_type" : stmt_ptr * int -> raw_column_type;
    fun column_type stmt col =
        withStmt(stmt, fn ptr =>
                 let val t = column_type_(ptr, col)
                 in  columnTypeFromRaw t
                 end)

    type raw_blob = MLton.Pointer.t      
    val column_bytes_ = _import "sqlite3_column_bytes" : stmt_ptr * int -> int;
    val column_blob_ = 
        _import "sqlite3_column_blob" : stmt_ptr * int -> raw_blob;
    val getWord8 = MLton.Pointer.getWord8

    fun column_blob stmt col = 
        withStmt(stmt, fn ptr =>
                 let val size = column_bytes_(ptr, col)
                     val raw = column_blob_(ptr, col)
                 in  Word8Vector.tabulate(size, fn i => getWord8(raw, i))
                 end)

    val column_int_ = _import "sqlite3_column_int" : stmt_ptr * int -> int;
    fun column_int stmt col = 
        withStmt(stmt, fn ptr => column_int_(ptr, col))

    val column_double_ = 
        _import "sqlite3_column_double" : stmt_ptr * int -> real;
    fun column_double stmt col = 
        withStmt(stmt, fn ptr => column_double_(ptr, col))
    val column_real = column_double


end (* structure Sqlite *)

