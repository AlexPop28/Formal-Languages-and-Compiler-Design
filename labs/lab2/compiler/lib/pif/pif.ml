open! Core

module Token = struct
  type t =
    | Reserved_word of string
    | Separator of string
    | Operator of string
    | Constant of string
    | Identifier of string
  [@@deriving sexp]
end

type t = (string * int) list ref [@@deriving sexp]

let create () = ref []
let add t ~token ~st_pos = t := (token, st_pos) :: !t
let to_list t = List.rev !t

let to_hum t =
  let buffer = Buffer.create 1024 in
  List.iter
    ~f:(fun (str_val, int_val) ->
      Buffer.add_string buffer (string_of_int int_val);
      Buffer.add_string buffer ": ";
      Buffer.add_string buffer str_val;
      Buffer.add_char buffer '\n')
    (to_list t);
  Buffer.contents buffer
;;
