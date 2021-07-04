let read_int str =
  let rec read' accum str =
    if String.length str = 0 then List.rev accum
    else match str.[0] with
    | '0'..'9' as v -> read' ( Char.escaped v :: accum ) (String.sub str 1 (String.length str - 1))
    | _ -> List.rev accum
  in
  let str' = read' [] str |> String.concat "" in
  (int_of_string str', String.sub str (String.length str') (String.length str - String.length str'))

let substring s pos =
  String.sub s pos (String.length s - pos)


let () =
  let argv = Sys.argv in
  if Array.length argv <> 2 then
    failwith "Invalid argument count"
  else ();
  let source = argv.(1) in
  let first_value, rest_source = read_int source in
  Printf.printf {|.intel_syntax noprefix
.globl main
main:
  mov rax, %d
|} first_value;

  let rec print_loop source =
    if String.length source = 0 then
      ()
    else
    match source.[0] with
    | '+' -> let value, source = read_int (substring source 1) in Printf.printf "  add rax, %d\n" value; print_loop source
    | '-' -> let value, source = read_int (substring source 1) in Printf.printf "  sub rax, %d\n" value; print_loop source
    | _ as v  -> failwith @@ Printf.sprintf "Invalid character %c" v
  in
  print_loop rest_source;
  print_endline "  ret"
