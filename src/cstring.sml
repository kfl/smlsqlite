(* smlsqlite - a binding of SQLite for Standard ML.                    *)
(*                                                                     *)
(* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         *)
(* All rights reserved. See the file LICENSE for license information.  *)

signature CString =
sig
    type cstring
    val fromString : string -> cstring

    type t
    val toString : t -> string option
    val toStringVal : t -> string

    val free : t -> unit
end

structure CString :> CString =
struct
    type cstring = string 
    fun fromString s = s ^ "\000"

    type t = MLton.Pointer.t

    val sub = MLton.Pointer.getWord8

    fun toVector t =
        let fun size i = if sub(t, i) = 0w0 then i
                         else size(i+1)
        in  if t <> MLton.Pointer.null then 
                SOME(Word8Vector.tabulate(size 0, fn i => sub(t, i)))
            else NONE
        end

    (* FIXME: Perhaps we ought to do some UTF-8 convertion *)
    val toString = (Option.map Byte.bytesToString) o toVector
    fun toStringVal t = Option.getOpt(toString t, "")


    val free = _import "free" : t -> unit;
end

