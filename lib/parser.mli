(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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

val parse : string -> string -> (expr, string) result

val parse_program :
  string -> string -> (expr * string list * expr list, string) result
