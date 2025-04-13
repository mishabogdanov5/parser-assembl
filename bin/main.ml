(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser.Parser_expr
open Parser.Parserl

let lw = Printf.sprintf "lw %s, %s\n"
let li = Printf.sprintf "li %s, %d\n"
let push = Printf.sprintf "addi sp, sp, -4\nsw %s, 0(sp)\n"

let binop op =
  Printf.sprintf
    "lw t0, 0(sp)\nlw t1, 4(sp)\n%s t2, t1, t0\naddi sp, sp, 4\nsw t2, 0(sp)\n"
    op

let sli_offsets expr =
  match expr with
  | Const n -> (
      match n with
      | 2 -> [ 1 ]
      | 4 -> [ 2 ]
      | 8 -> [ 3 ]
      | 16 -> [ 4 ]
      | 32 -> [ 5 ]
      | 64 -> [ 6 ]
      | 128 -> [ 7 ]
      | 256 -> [ 8 ]
      | 512 -> [ 9 ]
      | 1024 -> [ 10 ]
      | num ->
          let pow x n =
            let rec helper1 acc x = function
              | 0 -> acc
              | n when n mod 2 = 0 -> helper1 acc (x * x) (n / 2)
              | n -> helper1 (acc * x) x (n - 1)
            in
            helper1 1 x n
          in
          let rec helper r p ls =
            match p with
            | 0 -> if r = 1 then 0 :: ls else ls
            | oth ->
                let pr = pow 2 oth in
                if pr <= r then helper (r - pr) (p - 1) (oth :: ls)
                else helper r (p - 1) ls
          in
          helper num 31 [])
  | Add (_, _) | Sub (_, _) | Multi (_, _) | Div (_, _) | Var _ -> [ -1 ]

let sli_op expr =
  let offset = sli_offsets expr in
  match offset with
  | [ -1 ] ->
      Printf.sprintf
        "lw t0, 0(sp)\n\
         lw t1, 4(sp)\n\
         mul t2, t1, t0\n\
         addi sp, sp, 4\n\
         sw t2, 0(sp)\n"
  | [] -> Printf.sprintf "li t2, 0\naddi sp, sp, 4\nsw t2, 0(sp)\n"
  | _ ->
      let rec fill_offset acc ls =
        match ls with
        | [] -> List.fold_left (fun x y -> Printf.sprintf "%s" y ^ x) "" acc
        | h :: t ->
            fill_offset
              ((Printf.sprintf
                  "lw t0, 0(sp)\n\
                   slli t2, t0, %s\n\
                   addi sp, sp, 4\n\
                   sw t2, 0(sp)\n")
                 (string_of_int h)
              :: acc)
              t
      in
      fill_offset [] offset

let generate_expr expr =
  let rec helper = function
    | Add (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "add")
    | Sub (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "sub")
    | Multi (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (sli_op e1)
    | Div (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "div")
    | Const n -> li "t0" n ^ push "t0"
    | Var v -> lw "t0" v ^ push "t0"
  in
  helper expr

let generate_asm_prog (body, params, main_args) =
  let asm_expr = generate_expr body in
  let const_into_nums =
    List.map
      (fun e ->
        match e with Const n -> n | _ -> failwith "Error: wrong args in main")
      main_args
  in
  let asm_args =
    List.map2
      (fun v n -> Printf.sprintf "%s: .word %d" v n)
      params const_into_nums
  in
  let template =
    Printf.sprintf
      ".section .data\n\
      \ .align 8\n\
       %s\n\
       .section .text\n\
       .globl _start\n\
       _start:\n\
       %s\n\
       addi sp, sp, 4\n\
       mv a0, t2\n\
       call print_int\n\
       li a0, 0\n\
       li a7, 93\n\
       ecall\n"
  in
  template (String.concat "\n" asm_args) asm_expr ^ "\n"

let write_to_file filename str =
  let oc = open_out filename in
  Printf.fprintf oc "%s" str;
  close_out oc

let rec pp_term = function
  | Varl x -> x
  | Abs (x, t) -> Printf.sprintf "(\\%s.%s)" x (pp_term t)
  | App (t1, t2) ->
      let s1 = match t1 with Abs _ -> pp_term t1 | _ -> pp_term t1 in
      let s2 =
        match t2 with Varl _ -> pp_term t2 | _ -> "(" ^ pp_term t2 ^ ")"
      in
      Printf.sprintf "%s %s" s1 s2

let () =
  let input =
    if Array.length Sys.argv < 2 then exit 1
    else if Sys.argv.(1) = "lambda" then (
      let filename = Sys.argv.(2) in
      let ic = open_in filename in
      let input = really_input_string ic (in_channel_length ic) in
      close_in ic;
      input)
    else
      let filename = Sys.argv.(1) in
      let ic = open_in filename in
      let input = really_input_string ic (in_channel_length ic) in
      close_in ic;
      input
  in
  if Sys.argv.(1) = "lambda" then
    let parsed_lambda = parse_lambda input in
    match parsed_lambda with
    | Ok res -> print_string (pp_term res)
    | Error e -> Printf.printf "error: %s" e
  else
    let assoc = if Array.length Sys.argv < 3 then "left" else Sys.argv.(2) in
    let parsed_expr = parse_program input assoc in
    match parsed_expr with
    | Ok res ->
        let code = generate_asm_prog res in
        write_to_file "output.s" code
    | Error _ -> print_string "djfakjfd"
