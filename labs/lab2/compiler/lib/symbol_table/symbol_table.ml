type t = Indexed_hashset.t [@@deriving sexp]

let create = Indexed_hashset.create
let add_symbol = Indexed_hashset.add
let get_symbol = Indexed_hashset.get_by_index
let to_hum = Indexed_hashset.to_hum
