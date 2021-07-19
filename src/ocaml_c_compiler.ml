let gen_lval node =
  match node with
  | None      -> ()
  | Some node -> (
      match node.Node.kind with
      | Node.Lvar offset ->
          Printf.printf "  mov rax, rbp\n";
          Printf.printf "  sub rax, %d\n" offset;
          Printf.printf "  push rax\n"
      | _                ->
          Printf.eprintf "Left side of assignment is not variable";
          exit 1)

let rec gen node =
  match node with
  | None      -> ()
  | Some node -> (
      match node.Node.kind with
      | Num v       -> Printf.printf "   push %d\n" v
      | Node.Return ->
          gen node.lhs;
          Printf.printf "  pop rax\n";
          Printf.printf "  mov rsp, rbp\n";
          Printf.printf "  pop rbp\n";
          Printf.printf "  ret\n"
      | Node.Lvar _ ->
          gen_lval @@ Option.some node;
          Printf.printf "  pop rax\n";
          Printf.printf "  mov rax,[rax]\n";
          Printf.printf "  push rax\n"
      | Node.Assign ->
          gen_lval node.lhs;
          gen node.rhs;
          Printf.printf "  pop rdi\n";
          Printf.printf "  pop rax\n";
          Printf.printf "  mov [rax], rdi\n";
          Printf.printf "  push rdi\n"
      | _           ->
          gen node.lhs;
          gen node.rhs;

          Printf.printf "  pop rdi\n";
          Printf.printf "  pop rax\n";

          (match node.kind with
          | Node.Add       -> Printf.printf "  add rax, rdi\n"
          | Node.Sub       -> Printf.printf "  sub rax, rdi\n"
          | Node.Mul       -> Printf.printf "  imul rax, rdi\n"
          | Node.Equal     ->
              Printf.printf "  cmp rax, rdi\n";
              Printf.printf "  sete al\n";
              Printf.printf "  movzb rax, al\n"
          | Node.NotEqual  ->
              Printf.printf "  cmp rax, rdi\n";
              Printf.printf "  setne al\n";
              Printf.printf "  movzb rax, al\n"
          | Node.LessThan  ->
              Printf.printf "  cmp rax, rdi\n";
              Printf.printf "  setl al\n";
              Printf.printf "  movzb rax, al\n"
          | Node.LessEqual ->
              Printf.printf "  cmp rax, rdi\n";
              Printf.printf "  setle al\n";
              Printf.printf "  movzb rax, al\n"
          | Node.Div       ->
              (* idivは暗黙のうちにrdxとraxをとって、それらを合成して128bitとして、引数で割った後にraxに商を、rdxに余りをセットするという仕様になっている
                 cqoは、raxを128に伸ばしてraxとrdxにセットする、という動きをするため、ここで利用している *)
              Printf.printf "  cqo\n";
              Printf.printf "  idiv rdi\n"
          | _              -> ());
          Printf.printf "  push rax\n")

let () =
  let argv = Sys.argv in
  if Array.length argv <> 2 then failwith "Invalid argument count" else ();
  let source = argv.(1) in
  let tokenizer = Tokenizer.tokenize source in
  let program = Parser.parse tokenizer in

  Printf.printf {|.intel_syntax noprefix
.globl main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 208
|};

  List.iter
    (fun node ->
      gen (Some node);
      print_endline "  pop rax")
    program;

  (* 最後にraxにpopした式の値が残っているので、rbpを一つ前にcallのアドレスに戻してからretする *)
  print_endline "  mov rsp, rbp";
  print_endline "  pop rbp";
  print_endline "  ret"
