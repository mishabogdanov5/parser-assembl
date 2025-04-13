type g_type = Base of string | Fun of g_type * g_type

type lambda =
  | Var of string
  | Abs of string * g_type * lambda
  | App of lambda * lambda
  | Rec of string * g_type * lambda
