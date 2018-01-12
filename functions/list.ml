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

open Machine
open Grammar
open Utils

let name = "list"

(*
 * NOTE: for case 2, the sequence is enforced with the |> operator as OCaml
 * runs down the recursion before evaluating 'a'.
 *)

let rec run = function
  | Cons (a, Nil) ->
    Interpreter.eval a >>= fun a ->
    Ok (Cons (a, Nil))
  | Cons (a, b) ->
    Interpreter.eval a >>= fun a ->
    run b >>= fun b ->
    Ok (Cons (a, b))
  | t ->
    Interpreter.eval t >>= fun t ->
    Ok (Cons (t, Nil))

let hook = (name, run)
