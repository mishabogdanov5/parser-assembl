(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

type term = Varl of string | Abs of string * term | App of term * term

let ws = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)
let skip_spaces p = ws *> p <* ws
let skobki p = char '(' *> skip_spaces p <* char ')'

let var_parser =
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false)

let term =
  fix (fun term ->
      let abs_parser =
        char 'L' *> skip_spaces var_parser >>= fun var ->
        skip_spaces (char '.') *> term >>= fun body -> return (Abs (var, body))
      in

      let app_parser =
        let base_parser =
          choice [ (var_parser >>| fun x -> Varl x); skobki term; abs_parser ]
        in
        sep_by1 ws base_parser >>= function
        | hd :: tl -> return (List.fold_left (fun acc t -> App (acc, t)) hd tl)
        | [] -> fail "empty application"
      in

      skip_spaces @@ choice [ abs_parser; app_parser ])

let parse_lambda input = parse_string ~consume:All term input
