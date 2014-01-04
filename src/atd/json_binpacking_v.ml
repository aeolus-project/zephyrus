(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

type dimension = Json_binpacking_t.dimension

(** Items. *)
type size = Json_binpacking_t.size

type item_name = Json_binpacking_t.item_name

type item_arity = Json_binpacking_t.item_arity

(** Bins. *)
type item = Json_binpacking_t.item = {
  item_name (*atd name *): item_name;
  item_sizes (*atd sizes *): (dimension * size) list;
  item_arity (*atd arity *): item_arity
}

type bin_name = Json_binpacking_t.bin_name

type bin_cost = Json_binpacking_t.bin_cost

type bin_arity = Json_binpacking_t.bin_arity

(** Incompatibilities. *)
type bin = Json_binpacking_t.bin = {
  bin_name (*atd name *): bin_name;
  bin_sizes (*atd sizes *): (dimension * size) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

(** Binpacking problem. *)
type incompatibility = Json_binpacking_t.incompatibility

type binpacking_problem = Json_binpacking_t.binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list;
  binpacking_problem_incompatibilities (*atd incompatibilities *):
    incompatibility list
}

let validate_dimension = (
  (fun _ _ -> None)
)
let validate_size = (
  (fun _ _ -> None)
)
let validate_item_name = (
  (fun _ _ -> None)
)
let validate_item_arity = (
  (fun _ _ -> None)
)
let validate__1 = (
  fun _ _ -> None
)
let validate_item = (
  fun _ _ -> None
)
let validate_bin_name = (
  (fun _ _ -> None)
)
let validate_bin_cost = (
  (fun _ _ -> None)
)
let validate_bin_arity = (
  (fun _ _ -> None)
)
let validate_bin = (
  fun _ _ -> None
)
let validate__2 = (
  fun _ _ -> None
)
let validate_incompatibility = (
  validate__2
)
let validate__3 = (
  fun _ _ -> None
)
let validate__4 = (
  fun _ _ -> None
)
let validate__5 = (
  fun _ _ -> None
)
let validate_binpacking_problem = (
  fun _ _ -> None
)
let create_item 
  ~item_name
  ~item_sizes
  ~item_arity
  () =
  {
    item_name = item_name;
    item_sizes = item_sizes;
    item_arity = item_arity;
  }
let create_bin 
  ~bin_name
  ~bin_sizes
  ~bin_cost
  ~bin_arity
  () =
  {
    bin_name = bin_name;
    bin_sizes = bin_sizes;
    bin_cost = bin_cost;
    bin_arity = bin_arity;
  }
let create_binpacking_problem 
  ~binpacking_problem_items
  ~binpacking_problem_bins
  ~binpacking_problem_incompatibilities
  () =
  {
    binpacking_problem_items = binpacking_problem_items;
    binpacking_problem_bins = binpacking_problem_bins;
    binpacking_problem_incompatibilities = binpacking_problem_incompatibilities;
  }
