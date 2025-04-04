(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Multi of expr * expr
  | Div of expr * expr
  | Const of int
  | Var of string

val parse : string -> (expr, string) result
