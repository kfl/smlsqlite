(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

local structure S = Sqlite
      structure U = UtilDb
in

datatype tree = N of int * tree list

fun L i = N(i, nil)

fun isLeaf (N(_, subs)) = null subs

fun getID (N(i, _)) = i

fun cmp (N(i1, _), N(i2, _)) = Int.compare(i1, i2)

(* sort cripped from Listsort in mosmllib *) 
fun sort ordr []          = []
  | sort ordr (xs as [_]) = xs
  | sort ordr (xs as [x1, x2]) =
    (case ordr(x1, x2) of
	 GREATER => [x2, x1]
       | _       => xs)
  | sort ordr xs =
    let fun merge []       ys = ys
	  | merge (x1::xr) ys = 
	    let fun take x1 xr []       = x1 :: xr
		  | take x1 xr (y1::yr) = 
	            (case ordr(x1, y1) of 
			 LESS    => x1 :: take y1 yr xr
		       | _       => y1 :: take x1 xr yr)
	    in take x1 xr ys end
        fun mergepairs l1  []              k = [l1]
          | mergepairs l1 (ls as (l2::lr)) k =
            if k mod 2 = 1 then l1::ls
            else mergepairs (merge l1 l2) lr (k div 2)
	fun nextrun run []      = (run, [])
	  | nextrun run (xs as (x::xr)) =
	    if ordr(x, List.hd run) = LESS then (run, xs)
	    else nextrun (x::run) xr
        fun sorting []      ls r = List.hd(mergepairs [] ls 0)
          | sorting (x::xs) ls r =
	    let val (revrun, tail) = nextrun [x] xs
	    in sorting tail (mergepairs (List.rev revrun) ls (r+1)) (r+1) end
    in sorting xs [] 0 end;


fun normalise (N(i, subs)) = N(i, map normalise (sort cmp subs))
                             

(* A list represented as a tree, each node has one child, except for one 
   node that has zero children.
*)
fun makeListTree size =
    let val list = List.tabulate(size, fn i => i+1)
    in  foldl (fn(i, t) => N(i, [t])) (L 0) list
    end

(* The oppersite of makeListTree, each node has zero children, except for one
   node that has size children.
*)
fun makeFlatTree size = 
    let val children = List.tabulate(size, fn i => L(i+1))
    in  N(0, children)
    end

val i2s = Int.toString
val s2i = valOf o Int.fromString

val modifyLax = fn db => fn sql => (U.modifyDb db sql)
				   handle S.SQLiteError(status,_) => status

(* FIXME: do some error checking *)
fun dumpTreeToDb tablename tree db =
    let val insert_pre = "INSERT OR ROLLBACK INTO " ^ tablename ^ " VALUES("
        fun insert i p = U.modifyDb db (concat [insert_pre, i, "," , p, ");"]) 

        fun dumpNode parent (N(i, subs)) =
            let val id = i2s i
            in  insert id parent
              ; app (dumpNode id) subs
            end

    in  modifyLax db ("DROP TABLE "^tablename^";")
      ; U.modifyDb db "BEGIN TRANSACTION;"
      ; U.modifyDb db ("CREATE TABLE "^tablename^" (id INT PRIMARY KEY, parent INT);")
      ; U.modifyDb db ( "CREATE INDEX "^tablename^"_parentindex ON "^tablename
                      ^ " (parent);")
      ; dumpNode "NULL" tree
      ; U.modifyDb db "COMMIT TRANSACTION;"
    end

fun dumpToFile filename tablename tree =
    U.withDb filename (dumpTreeToDb tablename tree)


(* FIXME: do some error handling *)
fun readFromDb tablename db =
    let val select_pre = "SELECT id from " ^ tablename ^ " WHERE "

        fun select condition =
            let val query = concat(select_pre :: condition)
                val cids = U.foldDb db (fn ([cid], cids) => cid :: cids) 
				    [] query
            in  map readNode cids
            end

        and childrenOf id =  select ["parent = ", id, ";"]

        and readNode id =
            let val i = s2i id
                val subs = childrenOf id
            in  N(i, subs)
            end
      
    in  U.modifyDb db "BEGIN TRANSACTION;"
      ; hd (select [" parent ISNULL;"]) 
        before ignore(U.modifyDb db "COMMIT TRANSACTION;") 
    end              

fun readFile filename tablename =
    U.withDb filename (readFromDb tablename)

val t1 = N(4,
           [ N(2, [L 1, L 3])
           , N(6, [L 5, L 7])
           ])

val t2s = Time.toString
fun withTiming phase f arg =
    let val timer = Timer.startCPUTimer ()
	val res = f arg
	val {usr,sys,...} = Timer.checkCPUTimer timer
	val _ = print(concat[phase,": ",
		  "usr= ", t2s usr, " sys= ", t2s sys, "\n"])
    in  res
    end

fun test testtable tree =
    let val _ = print(testtable^"\n")
	val _ = withTiming "  dump" (dumpToFile "treetest.db" testtable) tree
        val tree2 = withTiming "  read" (readFile "treetest.db") testtable
    in  if tree = normalise tree2 
        then print "  After normalisation we get what we wrote.  Good.\n"
        else print "  We did not get back what we wrote.  Bad.\n"
    end handle S.SQLiteError(status, msg) => 
               print ("SQLite ERROR in "^ testtable ^ ": " ^ msg ^ " (" ^
                      S.statusToString status ^
                      ")\n")
                 
val _ = ( test "test1" t1
        ; test "flattreetest" (makeFlatTree 100000)
        ; test "listtreetest" (makeListTree 100000)
        )

end
