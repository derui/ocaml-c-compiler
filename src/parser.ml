module T = Tokenizer

let parse tokenizer =
  let rec add tokenizer =
    let node = mul tokenizer in

    let rec loop current =
      if T.consume tokenizer "+" then loop (Node.make ~lhs:current ~rhs:(mul tokenizer) Node.Add)
      else if T.consume tokenizer "-" then loop (Node.make ~lhs:current ~rhs:(mul tokenizer) Node.Sub)
      else current
    in
    loop node
  and primary tokenizer =
    if T.consume tokenizer "(" then (
      let node = equality tokenizer in
      T.expect tokenizer ")";
      node)
    else Node.make (Node.Num (T.expect_number tokenizer))
  and mul tokenizer =
    let node = unary tokenizer in

    let rec loop current =
      if T.consume tokenizer "*" then loop (Node.make ~lhs:current ~rhs:(unary tokenizer) Node.Mul)
      else if T.consume tokenizer "/" then loop (Node.make ~lhs:current ~rhs:(unary tokenizer) Node.Div)
      else current
    in
    loop node
  and unary tokenizer =
    if T.consume tokenizer "+" then primary tokenizer
    else if T.consume tokenizer "-" then Node.make ~lhs:(Node.make (Node.Num 0)) ~rhs:(primary tokenizer) Node.Sub
    else primary tokenizer
  and equality tokenizer =
    let node = relational tokenizer in

    let rec loop current =
      if T.consume tokenizer "==" then loop (Node.make ~lhs:current ~rhs:(relational tokenizer) Node.Equal)
      else if T.consume tokenizer "!=" then loop (Node.make ~lhs:current ~rhs:(relational tokenizer) Node.NotEqual)
      else current
    in
    loop node
  and relational tokenizer =
    let node = add tokenizer in

    let rec loop current =
      if T.consume tokenizer "<" then loop (Node.make ~lhs:current ~rhs:(add tokenizer) Node.LessThan)
      else if T.consume tokenizer "<=" then loop (Node.make ~lhs:current ~rhs:(add tokenizer) Node.LessEqual)
      else if T.consume tokenizer ">" then loop (Node.make ~rhs:current ~lhs:(add tokenizer) Node.LessThan)
      else if T.consume tokenizer ">=" then loop (Node.make ~rhs:current ~lhs:(add tokenizer) Node.LessEqual)
      else current
    in
    loop node
  in

  equality tokenizer
