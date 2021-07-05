type token_kind =
  | RESERVED
  | NUM
  | EOF
[@@deriving show]

type token = {
  kind : token_kind;
  mutable next : token option;
  value : int;
  raw : string;
}
[@@deriving show]

type t = { mutable token : token option }

let next t = t.token <- Option.map (fun t -> t.next) t.token |> Option.join

let error fmt = Printf.kfprintf (fun _ -> exit 1) stderr fmt

let consume t c =
  match t.token with
  | None       -> false
  | Some token -> (
      match token.kind with
      | RESERVED when token.raw.[0] = c ->
          next t;
          true
      | _ -> false)

let expect t op =
  match t.token with
  | None       -> error "end of tokens"
  | Some token -> (
      match token.kind with RESERVED when token.raw.[0] = op -> next t | _ -> error "expect failed '%c'" op)

let expect_number t =
  match t.token with
  | None -> error "end of tokens"
  | Some token when token.kind = NUM ->
      next t;
      token.value
  | _ -> error "token is not number"

let at_eof t = t.token |> Option.fold ~none:false ~some:(fun token -> token.kind = EOF)

let new_token ~cur ~raw ?(value = 0) kind =
  let token = { kind; raw; next = None; value } in
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

let tokenize str =
  let head = { kind = EOF; raw = ""; value = 0; next = None } in
  let current = ref head in

  let rec tokenize' source =
    if String.length source = 0 then ()
    else if Lib.Char.is_space source.[0] then tokenize' @@ Lib.String.substring source 1
    else
      match source.[0] with
      | ('+' | '-') as c ->
          current := new_token ~cur:!current ~raw:(Char.escaped c) RESERVED;
          tokenize' @@ Lib.String.substring source 1
      | '0' .. '9'       ->
          let value, rest = read_int source in
          current := new_token ~cur:!current ~raw:(string_of_int value) ~value NUM;
          tokenize' rest
      | c                -> error "can not tokenize: %c" c
  in
  tokenize' str;

  new_token ~cur:!current ~raw:"" EOF |> ignore;

  { token = head.next }
