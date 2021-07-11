let rec gen node =
  match node with
  | None      -> ()
  | Some node -> (
      match node.Node.kind with
      | Num v -> Printf.printf "   push %d\n" v
      | _     ->
          gen node.lhs;
          gen node.rhs;

          Printf.printf "  pop rdi\n";
          Printf.printf "  pop rax\n";

          (match node.kind with
          | Node.Add -> Printf.printf "  add rax, rdi\n"
          | Node.Sub -> Printf.printf "  sub rax, rdi\n"
          | Node.Mul -> Printf.printf "  imul rax, rdi\n"
          | Node.Div ->
              (* idivは暗黙のうちにrdxとraxをとって、それらを合成して128bitとして、引数で割った後にraxに商を、rdxに余りをセットするという仕様になっている
                 cqoは、raxを128に伸ばしてraxとrdxにセットする、という動きをするため、ここで利用している *)
              Printf.printf "  cqo\n";
              Printf.printf "  idiv rdi\n"
          | _        -> ());
          Printf.printf "  push rax\n")

let () =
  let argv = Sys.argv in
  if Array.length argv <> 2 then failwith "Invalid argument count" else ();
  let source = argv.(1) in
  let tokenizer = Tokenizer.tokenize source in
  let node = Parser.parse tokenizer in

  Printf.printf {|.intel_syntax noprefix
.globl main
main:
|};

  gen (Some node);

  (* stackのtopに値が残っているはずなので、それをraxにロードしてから関数の返り値とする *)
  Printf.printf "  pop rax\n";
  print_endline "  ret"
