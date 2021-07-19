type kind =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | Assign
  | NotEqual
  | LessThan
  | LessEqual
  | Return
  | Num       of int
  | Lvar      of int

type t = {
  kind : kind;
  lhs : t option;
  rhs : t option;
}

let make ?lhs ?rhs kind = { kind; lhs; rhs }
