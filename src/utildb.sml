(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

structure UtilDb :> UTIL_DB = 
struct

    structure S = Sqlite
    type db = S.db
    type status = S.status
    type sql = string

    fun withDb filename f =
	let val db = S.open_db filename
	in  (f db before S.close_db db)
            handle ?? => (S.close_db db; raise ??) 
	end

(*    fun foldDb db f init sql =
        let val state = ref init
            val exc = ref NONE
            fun wrap (values, colnames) =
                let val new = f(values, !state)
                in  state := new
                  ; true
                end handle ?? => (exc := SOME ??; false)
        in  case S.exec' db sql wrap of
                S.OK => (case !exc of
                             SOME error => raise error
                           | NONE       => !state)
              | status  => raise S.SQLiteError(status, S.errmsg db)
        end 

    fun modifyDb db sql = S.exec db sql NONE
*)

    fun withStmt db sql f =
	let val stmt = (S.prepare db sql)
	in  (f stmt before ignore(S.finalize stmt))
	    handle ?? => (S.finalize stmt; raise ??)
	end

    fun modifyDb db sql =
	let fun f stmt =
		case S.step stmt of 
		    S.DONE => S.DONE
		  | status => status
	in  withStmt db sql f
            handle S.SQLiteError(status, _) => status
	end

    fun foldDb db f init sql =
	let val c = S.column_count
	    val t = S.column_text
	    fun loop acc stmt =
		case S.step stmt of
		    S.DONE => acc
		  | S.ROW => loop (f(List.tabulate(c stmt,t stmt), acc)) stmt
		  | status => raise S.SQLiteError(status, S.errmsg db)
	in  withStmt db sql (loop init)
	end

    type blob = Word8Vector.vector
    datatype value = Integer of int
                   | Float   of real
                   | Text    of string
                   | Blob    of blob
                   | Null

    fun column_value stmt i =
        case S.column_type stmt i of
            S.INTEGER => Integer(S.column_int stmt i)
          | S.FLOAT   => Float(S.column_double stmt i)
          | S.TEXT    => Text(S.column_text stmt i)
          | S.BLOB    => Blob(S.column_blob stmt i)
          | S.NULL    => Null

    fun foldTabi db tabulate elem row init sql =
	let fun stepping init stmt =
                let val size = S.column_count stmt
                    fun conv i = elem(column_value stmt i, i)
                    fun loop acc =
		        case S.step stmt of
		            S.DONE => acc
		          | S.ROW  => loop(row(tabulate(size, conv), acc))
                          | status => raise S.SQLiteError(status, S.errmsg db)
                in  loop init 
                end
	in  withStmt db sql (stepping init)
	end

    fun foldVal db = foldTabi db List.tabulate #1

    type 'a getter = S.stmt -> int -> 'a
    val getInt  = S.column_int 
    val getText = S.column_text
    
    datatype row = S of { stmt  : S.stmt
                        , max   : int
                        , index : int
                        }
    fun incr (S{stmt, max, index=i}) = S{stmt=stmt, max=max, index=i+1}

    type ('a, 'b) trans          = 'a * row -> 'b * row
    type ('a, 'rest) cell_reader = ('a -> 'rest, 'rest) trans
    type 'a return               = 'a * row -> 'a

    fun state f stmt max = (f, S{stmt=stmt, max=max, index = 0})

    fun noNullCheckGetter get (f, state as S{stmt, max, index}) = 
        if index < max then (f (get stmt index), incr state)
        else raise Subscript

    fun drop (f, state as S{max, index, ...}) =
        if index < max then (f, incr state)
        else raise Subscript

    fun getter get (f, state as S{stmt, max, index}) = 
        if index < max then 
            case S.column_type stmt index of 
                S.NULL => drop (f NONE, state) 
              | _      => (f (SOME(get stmt index)), incr state)
        else raise Subscript
    val getter = noNullCheckGetter
    fun return (x, state as S{max, index, ...} ) =
        if index = max then x
        else raise Subscript

    fun int x  = getter getInt x
    fun text x = noNullCheckGetter getText x
    fun unit x = getter (fn _ => fn _ => ()) x
    val string = text
    fun real x = getter S.column_double x
    fun blob x = getter S.column_blob x
    fun value x = noNullCheckGetter column_value x
    fun option getter (f, state as S{stmt, max, index}) = 
        if index < max then
            case S.column_type stmt index of 
                S.NULL => drop (f NONE, state) 
              | _      => getter (fn x => f(SOME x), state)
        else raise Subscript

    infixr --> 
    fun (x --> y) arg = y (x arg)                        

    fun apply conv f stmt = return(conv(state f stmt (S.column_count stmt))) 

    fun foldT db schema sql f init =
	let fun stepping init stmt =
                let val size = S.column_count stmt
                    val conv = apply schema f
                    fun loop acc =
		        case S.step stmt of
		            S.DONE => acc
		          | S.ROW  => loop(conv stmt acc)
                          | status => raise S.SQLiteError(status, S.errmsg db)
                in  loop init 
                end
	in  withStmt db sql (stepping init)
	end

end (* structure UtilDb *)
