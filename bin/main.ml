(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser

(*let lw = Printf.sprintf "lw %s, %s\n"
let li = Printf.sprintf "li %s, %d\n"
let push = Printf.sprintf "addi sp, sp, -4\nsw %s, 0(sp)\n"

let binop op =
  Printf.sprintf
    "lw t0, 0(sp)\nlw t1, 4(sp)\n%s t2, t1, t0\naddi sp, sp, 4\nsw t2, 0(sp)\n"
    op
*)

let rec expr_to_string = function
  | Const c -> string_of_int c
  | Var x -> x
  | Add (x, y) -> "(" ^ expr_to_string x ^ "+" ^ expr_to_string y ^ ")"
  | Sub (x, y) -> "(" ^ expr_to_string x ^ "-" ^ expr_to_string y ^ ")"
  | Multi (x, y) -> "(" ^ expr_to_string x ^ "*" ^ expr_to_string y ^ ")"
  | Div (x, y) -> "(" ^ expr_to_string x ^ "/" ^ expr_to_string y ^ ")"

let print_prog (body, params, main_args) =
  Printf.printf "%s\n" (expr_to_string body);
  List.iter (fun s -> Printf.printf "%s " s) params;
  List.iter (fun e -> Printf.printf "%s " (expr_to_string e)) main_args

let print_res str =
  match parse_program str with
  | Ok res -> print_prog res
  | Error e -> Printf.printf "Error: %s" e

let () =
  let inp1 = read_line () in
  let inp2 = read_line () in
  print_res (inp1 ^ "\n" ^ inp2)
