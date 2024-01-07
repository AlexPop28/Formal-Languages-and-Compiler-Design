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

let from_hum s =
  String.split s ~on:'\n'
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun x -> String.length x > 0)
  |> List.map ~f:(fun row ->
    let row_entries = String.split ~on:':' row in
    match row_entries with
    | a :: b :: _ -> Ok (String.strip a, String.strip b)
    | _ -> Or_error.error_s [%message ("invalid format for pif" : string) (row : string)])
  |> List.map ~f:(fun processed_row ->
    let%bind.Or_error a, b = processed_row in
    match Int.of_string_opt a with
    | None -> Or_error.error_string "Entry coulnd't be cast to int"
    | Some x -> Ok (b, x))
  |> Or_error.all
  |> Or_error.map ~f:(fun x -> ref (List.rev x))
;;
