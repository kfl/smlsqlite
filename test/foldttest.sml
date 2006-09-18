(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

local
    structure U = UtilDb 
    infixr -->
    val op --> = U.-->
in

datatype value = datatype U.value

val dbname = "foldttest.db"



fun writeTable tablename =
    U.withDb dbname (fn db => app (ignore o U.modifyDb db)
             [ "DROP TABLE "^tablename^";"
             , "CREATE TABLE "^tablename^" (id INT PRIMARY KEY, parent INT);"
             , "INSERT INTO "^tablename^" VALUES( 1, 10 );"
             , "INSERT INTO "^tablename^" VALUES( 2, 20 );"
             , "INSERT INTO "^tablename^" VALUES( 42, NULL );"
             , "INSERT INTO "^tablename^" VALUES( 23, NULL );"
             ])

fun readTable tablename =
    let val query = "SELECT * from " ^ tablename ^ " ORDER BY id;"
        fun readRow id parent acc = (id, parent) :: acc
        fun readRows db = 
            U.foldT db (U.string --> U.option U.int) query 
                    readRow []
    in  U.withDb dbname readRows
    end



val test = ( writeTable "test"
           ; if readTable "test" =  [ ("42", NONE )
                                    , ("23", NONE )
                                    , ("2",  SOME 20 )
                                    , ("1",  SOME 10 )
                                    ]
             then print " foldTtest: SUCCESS\n"
             else print " foldTtest: FAILURE\n"
           )
end
