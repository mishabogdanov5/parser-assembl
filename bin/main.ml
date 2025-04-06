(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser

let lw = Printf.sprintf "lw %s, %s\n"
let li = Printf.sprintf "li %s, %d\n"
let push = Printf.sprintf "addi sp, sp, -4\nsw %s, 0(sp)\n"

let binop op =
  Printf.sprintf
    "lw t0, 0(sp)\nlw t1, 4(sp)\n%s t2, t1, t0\naddi sp, sp, 4\nsw t2, 0(sp)\n"
    op

let generate_expr expr =
  let rec helper = function
    | Add (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "add")
    | Sub (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "sub")
    | Multi (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "mul")
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

let () =
  let input =
    if Array.length Sys.argv < 2 then exit 1
    else
      let filename = Sys.argv.(1) in
      let ic = open_in filename in
      let input = really_input_string ic (in_channel_length ic) in
      close_in ic;
      input
  in
  let parsed_expr = parse_program input in
  match parsed_expr with
  | Ok res ->
      let code = generate_asm_prog res in
      write_to_file "output.s" code
  | Error _ -> print_string "djfakjfd"
