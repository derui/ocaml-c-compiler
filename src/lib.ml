module Char = struct
  include Stdlib.Char

  let is_space = function ' ' | '\t' | '\n' -> true | _ -> false

  let to_digit = function '0' .. '9' as v -> Some (Char.code v - Char.code '0') | _ -> None
end

module String = struct
  include Stdlib.String

  let substring s pos = String.sub s pos (String.length s - pos)
end
