type offset = int

type t = {
  name : string;
  offset : offset;
}

let offset_size : offset = 8

let make ~name ~base_offset = { name; offset = base_offset + offset_size }
