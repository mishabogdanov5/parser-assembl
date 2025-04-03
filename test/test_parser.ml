open Alcotest

let test_fact ()= 
  check int "default value" 720 (Parser.fact 6);
  check int "one" 1 (Parser.fact 1);
  check int "zero" 1 (Parser.fact 0)

let () = 
  run "Parser tests" [
      ("factorial", [
          test_case "Factorial of integer" `Quick test_fact;
      ]);
  ]
