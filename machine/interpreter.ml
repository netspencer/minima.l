(*
 * Copyright (c) 2018 Xavier R. Guérin <copyright@applepine.org>
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

open Grammar
open Utils

(*
 * Exception.
 *)

exception Throw of Grammar.t

(*
 * Global references.
 *)

let in_channel = ref ("stdin", stdin, Lexing.from_channel stdin)
let out_channel = ref ("stdout", stdout)

(*
 * Prefix management.
 *)

let prefix = ref "."

let get_prefix () =
  !prefix

let set_prefix v =
  prefix := v

(*
 * Concatenation.
 *)

let rec conc next = function
  | Cons (a, (Cons _ as b)) -> Cons (a, conc next b)
  | Cons (a, b) -> Cons (a, next)
  | a -> next

(*
 * Evaluation function.
 *)

let rec push ~closure args values =
  match args, values with
  (*
   * Pattern matching.
   *)
  | Any, _ -> Ok Nil
  | Nil, Nil -> Ok Nil
  (*
   * Symbol binding.
   *)
  | Symbol s, v ->
    let old = Cons (Symbol s, Cons (World.get ~closure s, Nil)) in
    World.set s v;
    Ok old
  (*
   * Recursive descent.
   *)
  | Cons (a0, a1), Cons (v0, v1) ->
    push ~closure a0 v0 >>= fun b0 ->
    push ~closure a1 v1 >>= fun b1 ->
    Ok (conc b0 b1)
  (*
   * Error scenarios.
   *)
  | a, b -> Error.unexpected (Cons (a, b))

and pop = function
  | Cons (Symbol s, Cons (v, rest)) -> World.set s v; pop rest
  | _ -> ()

and bind ~closure args values = match args, values with
  | Nil, _ -> Ok Nil
  | Cons (a0, args), Cons (v0, values) ->
    eval ~closure v0 >>= fun v0 ->
    bind ~closure args values >>= fun tl ->
    push ~closure a0 v0 >>= fun hd ->
    Ok (conc hd tl)
  | a0, v0 -> eval ~closure v0 >>= push ~closure a0

and exec ~closure values = function
  | Internal (_, fn) -> fn closure values
  | Function (_, Nil, body, closure) -> eval ~closure body
  | Function (_, args, body, closure) ->
    bind ~closure args values >>= fun s ->
    eval ~closure body
    |> fun r -> pop s;
    r
  | (Number _ as v)
  | (String _ as v) -> Ok (Cons (v, values))
  | l -> Error.cannot_execute l

and resolve ~closure = function
  | Symbol s -> Ok (World.get ~closure s)
  | l -> Ok l

and eval ~closure t =
  let eval_ = function
    | Cons (a, b) -> eval ~closure a >>= exec ~closure b
    | l -> resolve ~closure l
  in
  Trace.enter t >>= eval_ |> Trace.leave
