module T = Tokenizer

let parse tokenizer =
  let rec expr tokenizer =
    let node = mul tokenizer in

    let rec loop current =
      if T.consume tokenizer '+' then loop (Node.make ~lhs:current ~rhs:(mul tokenizer) Node.Add)
      else if T.consume tokenizer '-' then loop (Node.make ~lhs:current ~rhs:(mul tokenizer) Node.Sub)
      else current
    in
    loop node
  and primary tokenizer =
    if T.consume tokenizer '(' then (
      let node = expr tokenizer in
      T.expect tokenizer ')';
      node)
    else Node.make (Node.Num (T.expect_number tokenizer))
  and mul tokenizer =
    let node = primary tokenizer in

    let rec loop current =
      if T.consume tokenizer '*' then loop (Node.make ~lhs:current ~rhs:(primary tokenizer) Node.Mul)
      else if T.consume tokenizer '/' then loop (Node.make ~lhs:current ~rhs:(primary tokenizer) Node.Div)
      else current
    in
    loop node
  in
  expr tokenizer
