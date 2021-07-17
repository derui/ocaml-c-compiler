module T = Tokenizer

let find_lvar locals name = List.find_opt (fun v -> v.Lvar.name = name) locals

let parse tokenizer =
  let module S = Lib.String in
  let locals : Lvar.t list ref = ref [] in
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
    else
      let ident = T.consume_ident tokenizer in
      match ident with
      | Some token ->
          let name = token.T.raw in
          let lvar =
            find_lvar !locals name
            |> Option.fold
                 ~some:(fun lvar () -> lvar)
                 ~none:(fun () ->
                   let lvar =
                     Lvar.make ~name
                       ~base_offset:
                         (List.nth_opt !locals 0 |> Option.map (fun v -> v.Lvar.offset) |> Option.value ~default:0)
                   in
                   locals := lvar :: !locals;
                   lvar)
          in
          Node.make (Node.Lvar (lvar ()).Lvar.offset)
      | None       -> Node.make (Node.Num (T.expect_number tokenizer))
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
  and assign tokenizer =
    let node = equality tokenizer in

    if T.consume tokenizer "=" then Node.make ~lhs:node ~rhs:(assign tokenizer) Node.Assign else node
  and expr tokenizer = assign tokenizer
  and stmt tokenizer =
    let node = expr tokenizer in
    T.expect tokenizer ";";
    node
  and program tokenizer =
    let rec loop accum =
      if T.at_eof tokenizer then List.rev accum
      else
        let accum = stmt tokenizer :: accum in
        loop accum
    in
    loop []
  in

  program tokenizer
