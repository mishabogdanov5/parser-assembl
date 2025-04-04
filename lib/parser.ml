(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Multi of expr * expr
  | Div of expr * expr
  | Const of int
  | Var of string

let skip_spaces = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)

let int_p =
  skip_spaces *> take_while1 (function '0' .. '9' -> true | _ -> false)
  >>= fun s -> return (Const (int_of_string s))

let var_p =
  skip_spaces *> take_while1 (function 'a' .. 'z' -> true | _ -> false)
  >>= fun s -> return (Var s)

let skobki_p p = skip_spaces *> char '(' *> p <* char ')'
let add_op = skip_spaces *> char '+' *> return (fun x y -> Add (x, y))
let sub_op = skip_spaces *> char '-' *> return (fun x y -> Sub (x, y))
let multi_op = skip_spaces *> char '*' *> return (fun x y -> Multi (x, y))
let div_op = skip_spaces *> char '/' *> return (fun x y -> Div (x, y))

let rec left_assoc_parse elem op =
  elem >>= fun x ->
  op
  >>= (fun f -> left_assoc_parse elem op >>= fun y -> return (f x y))
  <|> return x

let expr =
  fix (fun expr ->
      let factor = choice [ int_p; var_p; skobki_p expr ] in
      let term = left_assoc_parse factor (multi_op <|> div_op) in
      left_assoc_parse term (add_op <|> sub_op))

let parse s = parse_string ~consume:All expr s
