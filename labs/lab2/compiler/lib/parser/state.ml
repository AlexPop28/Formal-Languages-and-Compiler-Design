open! Core

(* open Import *)
module Lr0_item = Lr0_item

type t = { items : Lr0_item.t Hash_set.t }

module Action = struct
  type t =
    | Shift
    | Reduce of (string * string list)
    | Accept
  [@@deriving sexp]
end

let get_all_lr0_items_right_dot_starting_with_symbol t symbol =
  Hash_set.filter t.items ~f:(fun item ->
    match item.right_dot with
    | hd :: _ when String.equal hd symbol -> true
    | _ -> false)
;;

let equal t1 t2 = Hash_set.equal t1.items t2.items
let sexp_of_t t = [%sexp (Hash_set.to_list t.items : Lr0_item.t list)]

let to_string_hum t =
  Hash_set.to_list t.items
  |> List.map ~f:Lr0_item.to_string_hum
  |> String.concat ~sep:" ; "
;;

let get_action t (grammar : Enhanced_grammar.t) =
  let action_list = ref [] in
  if Hash_set.find t.items ~f:(fun lr0_item -> not (List.is_empty lr0_item.right_dot))
     |> Option.is_some
  then action_list := [ Action.Shift ] @ !action_list;
  Hash_set.iter t.items ~f:(fun lr0_item ->
    if List.is_empty lr0_item.right_dot
    then action_list := [ Action.Reduce (lr0_item.lhp, lr0_item.left_dot) ] @ !action_list);
  let start = grammar.starting_symbol in
  if Hash_set.find t.items ~f:(fun lr0_item ->
       String.( = ) start lr0_item.lhp && List.is_empty lr0_item.right_dot)
     |> Option.is_some
  then action_list := [ Accept ];
  match !action_list with
  | [ action ] -> Ok action
  | action_list -> Error action_list
;;
