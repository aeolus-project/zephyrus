(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

type resource_name = Json_binpacking_t.resource_name

type resource_consume_arity = Json_binpacking_t.resource_consume_arity

(** Items. *)
type resource_provide_arity = Json_binpacking_t.resource_provide_arity

type item_name = Json_binpacking_t.item_name

type item_arity = Json_binpacking_t.item_arity

(** Bins. *)
type item = Json_binpacking_t.item = {
  item_name (*atd name *): item_name;
  item_consume (*atd consume *):
    (resource_name * resource_consume_arity) list;
  item_arity (*atd arity *): item_arity
}

type bin_name = Json_binpacking_t.bin_name

type bin_cost = Json_binpacking_t.bin_cost

type bin_arity = Json_binpacking_t.bin_arity

(** Binpacking problem. *)
type bin = Json_binpacking_t.bin = {
  bin_name (*atd name *): bin_name;
  bin_provide (*atd provide *): (resource_name * resource_provide_arity) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

type binpacking_problem = Json_binpacking_t.binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list
}

let validate_resource_name = (
  (fun _ _ -> None)
)
let validate_resource_consume_arity = (
  (fun _ _ -> None)
)
let validate_resource_provide_arity = (
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
let validate__2 = (
  fun _ _ -> None
)
let validate_bin = (
  fun _ _ -> None
)
let validate__3 = (
  fun _ _ -> None
)
let validate__4 = (
  fun _ _ -> None
)
let validate_binpacking_problem = (
  fun _ _ -> None
)
let create_item 
  ~item_name
  ~item_consume
  ~item_arity
  () =
  {
    item_name = item_name;
    item_consume = item_consume;
    item_arity = item_arity;
  }
let create_bin 
  ~bin_name
  ~bin_provide
  ~bin_cost
  ~bin_arity
  () =
  {
    bin_name = bin_name;
    bin_provide = bin_provide;
    bin_cost = bin_cost;
    bin_arity = bin_arity;
  }
let create_binpacking_problem 
  ~binpacking_problem_items
  ~binpacking_problem_bins
  () =
  {
    binpacking_problem_items = binpacking_problem_items;
    binpacking_problem_bins = binpacking_problem_bins;
  }
