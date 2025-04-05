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

type program = {
  func_name : string;
  params : string list;
  body : expr;
  main_args : expr list;
}

let ws = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)

let int_p =
  ws *> take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun s ->
  return (Const (int_of_string s))

let var_p =
  ws *> take_while1 (function 'a' .. 'z' -> true | _ -> false) >>= fun s ->
  return (Var s)

let skobki_p p = ws *> char '(' *> p <* char ')'
let add_op = ws *> char '+' *> return (fun x y -> Add (x, y))
let sub_op = ws *> char '-' *> return (fun x y -> Sub (x, y))
let multi_op = ws *> char '*' *> return (fun x y -> Multi (x, y))
let div_op = ws *> char '/' *> return (fun x y -> Div (x, y))

let rec left_assoc_parse elem op =
  elem >>= fun x ->
  op
  >>= (fun f -> left_assoc_parse elem op >>= fun y -> return (f x y))
  <|> return x

let expr_p =
  fix (fun expr ->
      let factor = choice [ int_p; var_p; skobki_p expr ] in
      let term = left_assoc_parse factor (multi_op <|> div_op) in
      left_assoc_parse term (add_op <|> sub_op))

let parse s = parse_string ~consume:All expr_p s

let fun_def_p =
  string "fun" *> ws
  *> ( take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
     >>= fun name ->
       ws *> many1 (var_p >>| function Var s -> s | _ -> failwith "impossible")
       >>= fun params ->
       ws *> char '=' *> ws *> expr_p >>= fun body -> return (name, params, body)
     )

let main_p =
  string "fun main =" *> ws
  *> ( take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
     >>= fun func_name ->
       ws *> many1 (expr_p <* ws) >>= fun args -> return (func_name, args) )

let program_p =
  fun_def_p >>= fun (func_name, params, body) ->
  ws *> main_p >>= fun (main_func, main_args) ->
  if func_name = main_func then return { func_name; params; body; main_args }
  else fail "Main must call the defined function"

let parse_program input =
  match parse_string ~consume:All program_p input with
  | Ok program -> Ok (program.body, program.params, program.main_args)
  | Error e -> Error ("Parse error: " ^ e)
