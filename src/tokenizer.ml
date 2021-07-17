type token_kind =
  | RESERVED
  | NUM
  | IDENT
  | EOF
[@@deriving show]

type loc = int [@@deriving show]

type token = {
  kind : token_kind;
  mutable next : token option;
  value : int;
  raw : string;
  loc : loc;
}
[@@deriving show]

type t = {
  mutable token : token option;
  original_source : string;
}

let next t = t.token <- Option.map (fun t -> t.next) t.token |> Option.join

let error source loc fmt =
  Printf.fprintf stderr "%s\n" source;
  Printf.fprintf stderr "%*s" loc " ";
  Printf.fprintf stderr "^ ";
  Printf.kfprintf (fun _ -> exit 1) stderr fmt

let consume t str =
  match t.token with
  | None       -> false
  | Some token -> (
      match token.kind with
      | RESERVED when token.raw = str ->
          next t;
          true
      | _ -> false)

let consume_ident t =
  match t.token with
  | None       -> None
  | Some token -> (
      match token.kind with
      | IDENT ->
          next t;
          Some token
      | _     -> None)

let expect t op =
  match t.token with
  | None       -> error t.original_source 0 "end of tokens"
  | Some token -> (
      match token.kind with
      | RESERVED when token.raw = op -> next t
      | _ -> error t.original_source token.loc "expect failed '%s'" op)

let expect_number t =
  match t.token with
  | None -> error t.original_source 0 "end of tokens"
  | Some token when token.kind = NUM ->
      next t;
      token.value
  | Some token -> error t.original_source token.loc "token is not number"

let at_eof t = t.token |> Option.fold ~none:false ~some:(fun token -> token.kind = EOF)

let new_token ~cur ~raw ?(value = 0) ~loc kind =
  let token = { kind; raw; next = None; value; loc } in
  cur.next <- Some token;
  token

let read_int str =
  let rec read' accum str =
    if String.length str = 0 then List.rev accum
    else
      match str.[0] with
      | '0' .. '9' as v -> read' (Char.escaped v :: accum) (String.sub str 1 (String.length str - 1))
      | _               -> List.rev accum
  in
  let str' = read' [] str |> String.concat "" in
  (int_of_string str', String.sub str (String.length str') (String.length str - String.length str'))

let read_ident str =
  let rec read' accum str =
    if String.length str = 0 then List.rev accum
    else
      match str.[0] with
      | 'a' .. 'z' as v -> read' (Char.escaped v :: accum) (String.sub str 1 (String.length str - 1))
      | _               -> List.rev accum
  in
  let str' = read' [] str |> String.concat "" in
  (str', String.sub str (String.length str') (String.length str - String.length str'))

let tokenize str =
  let head = { kind = EOF; raw = ""; value = 0; next = None; loc = 0 } in
  let current = ref head in
  let module S = Lib.String in
  let rec tokenize' source read_chars =
    if String.length source = 0 then ()
    else if Lib.Char.is_space source.[0] then tokenize' (Lib.String.substring source 1) (succ read_chars)
    else
      match source.[0] with
      | ('+' | '-' | '*' | '/' | '(' | ')' | ';') as c ->
          current := new_token ~cur:!current ~raw:(Char.escaped c) ~loc:read_chars RESERVED;
          tokenize' (Lib.String.substring source 1) (succ read_chars)
      | '0' .. '9' ->
          let value, rest = read_int source in
          current := new_token ~cur:!current ~raw:(string_of_int value) ~value ~loc:read_chars NUM;
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | 'a' .. 'z' ->
          let ident, rest = read_ident source in
          current := new_token ~cur:!current ~raw:ident ~loc:read_chars IDENT;
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source "==" ->
          current := new_token ~cur:!current ~raw:"==" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source "=" ->
          current := new_token ~cur:!current ~raw:"=" ~loc:read_chars RESERVED;
          let rest = S.substring source 1 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source "!=" ->
          current := new_token ~cur:!current ~raw:"!=" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source ">=" ->
          current := new_token ~cur:!current ~raw:">=" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source ">" ->
          current := new_token ~cur:!current ~raw:">" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source "<=" ->
          current := new_token ~cur:!current ~raw:"<=" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | _ when S.start_with source "<" ->
          current := new_token ~cur:!current ~raw:"<" ~loc:read_chars RESERVED;
          let rest = S.substring source 2 in
          tokenize' rest (read_chars + (String.length source - String.length rest))
      | c -> error str read_chars "can not tokenize: %c" c
  in
  tokenize' str 0;

  new_token ~cur:!current ~raw:"" ~loc:(String.length str - 1) EOF |> ignore;

  { token = head.next; original_source = str }
