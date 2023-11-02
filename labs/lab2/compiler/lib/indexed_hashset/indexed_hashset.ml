open! Core

type t = string option array [@@deriving sexp]

let size = 666_013
let create () = Array.init size ~f:(fun _ -> None)

let rec find_pos t pos ~f =
  if f t.(pos) then pos
  else
    let next_pos = (pos + 1) % size in
    find_pos t next_pos ~f

let hash key =
  let value = ref 0 in
  String.iter key ~f:(fun c ->
      value := !value * 257;
      value := !value + Char.to_int c;
      value := !value % size);
  !value

let find_key_or_empty_slot t key =
  let hashed_key = hash key in
  let f elem = match elem with None -> true | Some k -> String.(k = key) in
  find_pos t hashed_key ~f

(* let get_index t key = *)
(*   let pos = find_key_or_empty_slot t key in *)
(*   let%bind.Option _ = t.(pos) in *)
(*   Some pos *)

let add t key =
  let pos = find_key_or_empty_slot t key in
  if Option.is_none t.(pos) then t.(pos) <- Some key;
  pos

let get_by_index t index =
  if index < 0 || index >= size then None else t.(index)
