open Alcotest
open Parser

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
    ("2 + 3 - 10", Add (Const 2, Sub (Const 3, Const 10)));
    ("5 * (2 + 4) - 1", Sub (Multi (Const 5, Add (Const 2, Const 4)), Const 1));
    ( "10 + 6 / 3 + (2 - 4) * a",
      Add
        ( Const 10,
          Add (Div (Const 6, Const 3), Multi (Sub (Const 2, Const 4), Var "a"))
        ) );
    ( "a * b + (c - d) * e",
      Add (Multi (Var "a", Var "b"), Multi (Sub (Var "c", Var "d"), Var "e")) );
  ]

let () =
  let test_parser (input, expected) =
    let test_name = Printf.sprintf "Parsing: %s" input in
    test_case test_name `Quick (fun () ->
        match parse input "left" with
        | Ok actual ->
            if not (expr_equal actual expected) then
              Alcotest.failf "Expected %s, actual %s" (expr_to_string expected)
                (expr_to_string actual)
        | Error e -> Alcotest.failf "Parse fail: %s" e)
  in
  run "Parser Tests" [ ("Exprs", List.map test_parser test_cases) ]
