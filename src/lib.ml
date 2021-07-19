module Char = struct
  include Stdlib.Char

  let is_space = function ' ' | '\t' | '\n' -> true | _ -> false

  let to_digit = function '0' .. '9' as v -> Some (Char.code v - Char.code '0') | _ -> None

  let is_alnum = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> true
end

module String = struct
  include Stdlib.String

  let substring s pos = String.sub s pos (String.length s - pos)

  let start_with s ?(pos = 0) str =
    let str_len = String.length str in
    assert (str_len > 0);
    let substr_len = String.length s - pos in
    assert (substr_len >= 0);
    let substr = String.sub s pos @@ min substr_len str_len in
    substr = str

  let safe_get str pos = if pos < 0 || length str <= pos then None else Some (get str pos)
end
