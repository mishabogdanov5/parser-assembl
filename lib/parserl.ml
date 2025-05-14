(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

type term =
  | Value of string
  | Var of string
  | App of term * term
  | Abs of string * term
  | Let of string * term * term
  | Rec of string * term * term

let spaces =
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let is_reserved = function "let" | "in" | "fun" | "rec" -> true | _ -> false

let value_p =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun s -> return s

let value = value_p >>| fun num -> Value num

let identifier =
  take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  >>= fun s ->
  if is_reserved s then fail ("Reserved keyword: " ^ s) else return s

let var = identifier >>| fun s -> Var s
let parens p = char '(' *> spaces *> p <* spaces <* char ')'

let expr =
  fix (fun expr ->
      let let_expr =
        string "let" *> spaces *> identifier >>= fun name ->
        spaces *> char '=' *> spaces *> expr >>= fun value ->
        spaces *> string "in" *> spaces *> expr >>= fun body ->
        return (Let (name, value, body))
      in

      let let_rec_expr =
        string "let" *> spaces *> string "rec" *> spaces *> identifier
        >>= fun name ->
        spaces *> char '=' *> spaces *> expr >>= fun value ->
        spaces *> string "in" *> spaces *> expr >>= fun body ->
        return (Rec (name, value, body))
      in

      let abs_expr =
        string "fun" *> spaces *> identifier >>= fun param ->
        spaces *> string "->" *> spaces *> expr >>= fun body ->
        return (Abs (param, body))
      in

      let app_expr =
        many1 (spaces *> (parens expr <|> var <|> value)) >>| function
        | hd :: tl -> List.fold_left (fun acc t -> App (acc, t)) hd tl
        | [] -> failwith "Empty application"
      in

      spaces *> (let_expr <|> let_rec_expr <|> abs_expr <|> app_expr) <* spaces)

let parse str = parse_string ~consume:All expr str

let pp_lambda expr =
  let rec printer = function
    | Value vl -> "Value " ^ vl
    | Var name -> "Var " ^ name
    | Abs (name, term) -> "Abs (" ^ name ^ ", " ^ printer term ^ ")"
    | App (term1, term2) -> "App (" ^ printer term1 ^ ", " ^ printer term2 ^ ")"
    | Let (name, term1, term2) ->
        "Let(" ^ name ^ ", " ^ printer term1 ^ ", " ^ printer term2 ^ ")"
    | Rec (name, term1, term2) ->
        "Let Rec(" ^ name ^ ", " ^ printer term1 ^ ", " ^ printer term2 ^ ")"
  in
  printer expr

let parse_program s =
  match parse s with
  | Ok result -> Printf.printf "%s" (pp_lambda result)
  | Error e -> Printf.printf "%s" e

let%expect_test "parser-test-1" =
  parse_program "fun x -> x";
  [%expect {|Abs (x, Var x)|}]

let%expect_test "parser-test-2" =
  parse_program "fun x -> y";
  [%expect {|Abs (x, Var y)|}]

let%expect_test "parser-test-3" =
  parse_program "let id = fun x -> x in fun y -> y";
  [%expect {|Let(id, Abs (x, Var x), Abs (y, Var y))|}]

let%expect_test "parser-test-4" =
  parse_program "let id = fun x -> x in id y";
  [%expect {|Let(id, Abs (x, Var x), App (Var id, Var y))|}]

let%expect_test "parser-test-5" =
  parse_program "x y";
  [%expect {|App (Var x, Var y)|}]

let%expect_test "parser-test-6" =
  parse_program "let a = let rec fact = (fun x -> x) y in fact z in a c";
  [%expect
    {|Let(a, Let Rec(fact, App (Abs (x, Var x), Var y), App (Var fact, Var z)), App (Var a, Var c))|}]

let%expect_test "parser-test-7" =
  parse_program "let rec f = let rec g = fun x -> g x in g 1 in f g";
  [%expect {| Let Rec(f, Let Rec(g, Abs (x, App (Var g, Var x)), App (Var g, Value 1)), App (Var f, Var g)) |}]
