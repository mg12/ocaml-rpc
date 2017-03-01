(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

open Rpc


let rec bson_of_rpc t =
  match t with
  | Int i64   -> Bson.Int64 i64
  | Int32 i32 -> Bson.Int32 i32
  | Bool b    -> Bson.Boolean b
  | Float f   -> Bson.Double f
  | String s  -> Bson.String s
  | DateTime dt -> Bson.JSCode dt (* only to distinguish from String *)
  | Enum xs   -> Bson.Document (  (* bson sub-document with label "enum" *)
                   Bson.add_element "enum" (Bson.Array (List.map (fun x-> bson_of_rpc x) xs)) Bson.empty
                 ) 
  | Dict xs   -> Bson.Document (  (*  bson sub-document with label "dict" *)
                   Bson.add_element "dict" (Bson.Document (List.map (fun (s,x)-> (s, bson_of_rpc x)) xs)) Bson.empty
                 )
  | Null      -> Bson.create_null ()

let to_string t = Bson.encode (Bson.add_element "bson" (bson_of_rpc t) Bson.empty)

exception Bson_unknown
let rec rpc_of_bson x =
  match x with
  | Bson.Int64 i64 -> Int i64
  | Bson.Int32 i32 -> Int32 i32
  | Bson.Boolean b -> Bool b
  | Bson.Double  f -> Float f
  | Bson.String  s -> String s
  | Bson.JSCode  dt -> DateTime dt
  | Bson.Document (("enum", (Bson.Array xs))::_)  -> Enum (List.map (fun x->rpc_of_bson x) xs)
  | Bson.Document (("dict", (Bson.Document xs))::_)-> Dict (List.map (fun (s,x)->s,rpc_of_bson x)
                      (List.rev xs) (* List.rev works around a bug in Bson.Document that returns xs in the reversed order *)
                  )
  | Bson.Null _ -> Null
  | _ -> raise Bson_unknown

let of_string x =
  match Bson.decode x with
  | ("bson",el)::_->rpc_of_bson el
  | _ -> raise Bson_unknown 


(* TODO, not necessary currently *)
let to_fct x f = ()
let of_fct f = Rpc.Null

let to_a ~empty ~append x = empty ()
let of_a ~next_char x = Rpc.Null

let string_of_call x = ""
let call_of_string x = Rpc.call "name" []

let string_of_response x = ""
let response_of_string x = Rpc.failure Rpc.Null
let response_of_in_channel ch = Rpc.failure Rpc.Null

let a_of_response ~empty ~append x = empty ()

