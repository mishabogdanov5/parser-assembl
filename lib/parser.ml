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

let rec right_assoc_parse elem op =
  elem >>= fun x ->
  op
  >>= (fun f -> right_assoc_parse elem op >>= fun y -> return (f y x))
  <|> return x

let expr_p assoc =
  fix (fun expr ->
      let factor = choice [ int_p; var_p; skobki_p expr ] in
      let term =
        (if assoc = "left" then left_assoc_parse factor
         else right_assoc_parse factor)
          (multi_op <|> div_op)
      in
      (if assoc = "left" then left_assoc_parse term else right_assoc_parse term)
        (add_op <|> sub_op))

let parse s assoc = parse_string ~consume:All (expr_p assoc) s

let fun_def_p assoc =
  string "fun" *> ws
  *> ( take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
     >>= fun name ->
       ws *> many1 (var_p >>| function Var s -> s | _ -> failwith "impossible")
       >>= fun params ->
       ws *> char '=' *> ws *> expr_p assoc >>= fun body ->
       return (name, params, body) )

let main_p assoc =
  string "fun main =" *> ws
  *> ( take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
     >>= fun func_name ->
       ws *> many1 (expr_p assoc <* ws) >>= fun args -> return (func_name, args)
     )

let program_p assoc =
  fun_def_p assoc >>= fun (func_name, params, body) ->
  ws *> main_p assoc >>= fun (main_func, main_args) ->
  if func_name = main_func then return { func_name; params; body; main_args }
  else fail "Main must call the defined function"

let parse_program input assoc =
  match parse_string ~consume:All (program_p assoc) input with
  | Ok program -> Ok (program.body, program.params, program.main_args)
  | Error e -> Error ("Parse error: " ^ e)
