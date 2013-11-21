(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

type resource_name = string

type resource_consume_arity = int

(** Items. *)
type resource_provide_arity = int

type item_name = string

type item_arity = int

(** Bins. *)
type item = {
  item_name (*atd name *): item_name;
  item_consume (*atd consume *):
    (resource_name * resource_consume_arity) list;
  item_arity (*atd arity *): item_arity
}

type bin_name = string

type bin_cost = int

type bin_arity = int

(** Binpacking problem. *)
type bin = {
  bin_name (*atd name *): bin_name;
  bin_provide (*atd provide *): (resource_name * resource_provide_arity) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

type binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list
}
