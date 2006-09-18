(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

structure Callbacktable :> Callbacktable =
struct

structure A  = Array

infix 9 !!
infix ::=
val op !! = A.sub
fun (arr, i) ::= v = A.update(arr, i, v)

type key = int 

type 'a bucketlist = (key * 'a) list

type 'a table =
     { max_len : int                      (* max length of a bucket *)
     , data    : 'a bucketlist array      (* the buckets *)
     }

type 'a t = 'a table ref

fun new initial_size = ref { max_len = 3, data = A.array(initial_size, nil) }

fun resize (h as ref {data, max_len}) =
    let val len    = A.length data
        val newlen = len+len+1
        val newdata = A.array(newlen, nil)
        fun dispatch [] = ()
          | dispatch ((x as (k, v)) :: rest) =
            let val () = dispatch rest
                val i  = k mod newlen
            in (newdata, i) ::= x :: newdata !! i
            end
    in  A.app dispatch data
      ; h := { data = newdata, max_len = 2 * max_len}
    end


fun bucket_too_long n bucket =
    n < 0 orelse
    case bucket of
        []        => false
      | _ :: rest => bucket_too_long (n-1) rest

fun insert (h as ref {data, max_len}) (x as (key, _)) =
    let fun insert_bucket [] = [x]
          | insert_bucket ((y as (k, _)) :: next) =
            if k = key then x :: next
            else y :: insert_bucket next
        val i = key mod (A.length data)
        val bucket = insert_bucket (data !! i)
    in  (data, i) ::= bucket
      ; if bucket_too_long max_len bucket then resize h else ()
    end


fun remove (h as ref {data, max_len}) key =
    let fun remove_bucket [] = []
          | remove_bucket ((k, v) :: next) =
            if k = key then next
            else (k, v) :: remove_bucket next
        val i = key mod (A.length data)
    in  (data, i) ::= remove_bucket (data !! i)
    end


fun peek (h as ref {data, max_len}) key =
    let fun peek [] = NONE
          | peek ((k, d) :: rest) =
            if key = k then SOME d 
	    else peek rest
    in  peek (data !! (key mod (A.length data)))
    end

end
