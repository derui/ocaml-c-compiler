type kind =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | Num       of int

type t = {
  kind : kind;
  lhs : t option;
  rhs : t option;
}

let make ?lhs ?rhs kind = { kind; lhs; rhs }
