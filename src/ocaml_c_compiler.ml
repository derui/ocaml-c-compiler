let () =
  let argv = Sys.argv in
  if Array.length argv <> 2 then
    failwith "Invalid argument count"
  else ();
  let value = argv.(1) |> int_of_string in
  Printf.printf {|.intel_syntax noprefix
.globl main
main:
  mov rax, %d
  ret
|} value
