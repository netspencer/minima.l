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

let name = "in"

let process closure = function
  | Nil, Cons (prg, Nil) ->
    let old = !Interpreter.in_channel in
    let slx = Sedlexing.Utf8.from_channel stdin in
    let buf = Syntax.create_lexbuf slx in
    Interpreter.in_channel := ("stdin", stdin, buf);
    begin try
        let res = Interpreter.eval closure prg in
        Interpreter.in_channel := old;
        res
      with e ->
        Interpreter.in_channel := old;
        raise e
    end
  | String filename, Cons (prg, Nil) when String.length filename > 0 ->
    begin try
      let old = !Interpreter.in_channel in
      let dsc = Unix.openfile filename [ Unix.O_RDONLY ] 0o440 in
      let chn = Unix.in_channel_of_descr dsc in
      let slx = Sedlexing.Utf8.from_channel chn in
      let buf = Syntax.create_lexbuf ~file:filename slx in
      Interpreter.in_channel := (filename, chn, buf);
      begin try
          let res = Interpreter.eval closure prg in
          Interpreter.in_channel := old;
          Unix.close dsc;
          res
        with e ->
          Interpreter.in_channel := old;
          Unix.close dsc;
          raise e
      end
    with Unix.Unix_error _ ->
      Ok Nil
  end
  | a, b -> Error.undefined (Cons (a, b))

let run closure = function
  | Cons (fn, prg) -> Interpreter.eval closure fn >>= fun fn -> process closure (fn, prg)
  | t -> Error.undefined t

let hook = (name, run)
