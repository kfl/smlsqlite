(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

signature Callbacktable =
sig
    type key = int
    type 'a t
    val new : int -> 'a t
    val insert : 'a t -> key * 'a -> unit
    val peek : 'a t -> key -> 'a option
    val remove : 'a t -> key -> unit
end
