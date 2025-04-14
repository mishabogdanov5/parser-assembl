open Alcotest
open Parser.Parser_expr

let rec expr_to_string = function
  | Const c -> string_of_int c
  | Var x -> x
  | Add (x, y) -> "(" ^ expr_to_string x ^ "+" ^ expr_to_string y ^ ")"
  | Sub (x, y) -> "(" ^ expr_to_string x ^ "-" ^ expr_to_string y ^ ")"
  | Multi (x, y) -> "(" ^ expr_to_string x ^ "*" ^ expr_to_string y ^ ")"
  | Div (x, y) -> "(" ^ expr_to_string x ^ "/" ^ expr_to_string y ^ ")"

let expr_equal a b = expr_to_string a = expr_to_string b

let test_cases =
  [
    ("2 + 3 - 10", Const (-5), "left");
    ("5 * (2 + 4) - 1", Const 29, "left");
    ( "10 + 6 / 3 + (2 - 4) * a",
      Add (Const 10, Add (Const 2, Multi (Const (-2), Var "a"))),
      "left" );
    ( "a * b + (c - d) * e",
      Add (Multi (Var "a", Var "b"), Multi (Sub (Var "c", Var "d"), Var "e")),
      "left" );
    ( "(5 / (7 + 3)) * 8 + a * b",
      Add (Multi (Var "b", Var "a"), Const 16),
      "right" );
  ]

let () =
  let test_parser (input, expected, assoc) =
    let test_name = Printf.sprintf "Parsing: %s" input in
    test_case test_name `Quick (fun () ->
        match parse input assoc with
        | Ok actual ->
            if not (expr_equal actual expected) then
              Alcotest.failf "Expected %s, actual %s" (expr_to_string expected)
                (expr_to_string actual)
        | Error e -> Alcotest.failf "Parse fail: %s" e)
  in
  run "Parser Tests" [ ("Exprs", List.map test_parser test_cases) ]
