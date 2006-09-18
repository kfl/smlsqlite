open Sqlite

fun main args =
    case args of
	[file,sql] => 
	    let 
		fun f (values,columns) =
		    ( List.app (fn (v,c) => print(c^"="^getOpt(v,"NULL")^"\n"))
		               (ListPair.zip(values,columns))
                    ; true
                    )
		val db = open_db file
		val status = exec db sql f
	    in  close_db db end
      | [_,file,sql] => 
	    let 
		fun getList get num stmt = 
		    let fun loop n acc =
			    if n = num then rev acc
			    else loop (n+1) (get stmt n :: acc)
		    in  loop 0 []
		    end
		fun getColumns stmt = getList column_name (column_count stmt) stmt
		fun getColTypes stmt = getList column_decltype (column_count stmt) stmt
		fun getRow stmt = getList column_text (data_count stmt) stmt
			
		fun loop stmt =
		    let val status = step stmt
		    in  if status = DONE then finalize stmt
			else if status = ROW then
			    let val row = getRow stmt
				val _ = List.app (print o (fn s => getOpt(s,"NULL"))) row
				val _ = print"\n"
			    in  loop stmt
			    end
			else raise Fail("error stop")
		    end
		val db = open_db file
		val (stmt,_) = prepare db sql
		val _ = List.app print (getColumns stmt)
		val _ = List.app (print o (fn s => getOpt(s,"NULL"))) (getColTypes stmt)
		val _ = loop stmt
	    in  close_db db end
      | _ => print"Usage: testdb FILE SQL\n"

val _ = main (CommandLine.arguments())
