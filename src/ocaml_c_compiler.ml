let () =
  let argv = Sys.argv in
  if Array.length argv <> 2 then failwith "Invalid argument count" else ();
  let source = argv.(1) in
  let token = Tokenizer.tokenize source in

  Printf.printf {|.intel_syntax noprefix
.globl main
main:
  mov rax, %d
|} @@ Tokenizer.expect_number token;

  let rec print_loop token : unit =
    if Tokenizer.at_eof token then ()
    else (
      if Tokenizer.consume token '+' then Printf.printf "  add rax, %d\n" @@ Tokenizer.expect_number token
      else (
        Tokenizer.expect token '-';
        Printf.printf "  sub rax, %d\n" @@ Tokenizer.expect_number token);
      print_loop token)
  in
  print_loop token;
  print_endline "  ret"
