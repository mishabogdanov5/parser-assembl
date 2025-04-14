(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type term = Varl of string | Abs of string * term | App of term * term

val parse_lambda : string -> (term, string) result
