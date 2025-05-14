(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type term =
  | Value of string
  | Var of string
  | App of term * term
  | Abs of string * term
  | Let of string * term * term
  | Rec of string * term * term

val parse : string -> (term, string) result
val pp_lambda : term -> string
