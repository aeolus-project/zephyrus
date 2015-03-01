(* Auto-generated from "json_binpacking.atd" *)


(** Resources. *)

(** Items. *)
type size = int

type repository_name = string

type item_name = string

type item_arity = int

type dimension = string

(** Bins. *)
type item = {
  item_name (*atd name *): item_name;
  item_sizes (*atd sizes *): (dimension * size) list;
  item_arity (*atd arity *): item_arity
}

type incompatibility = item_name list

(** Binpacking problem. *)
type incompatibilities = incompatibility list

type bin_name = string

type bin_cost = int

type bin_arity = int

(** Incompatibilities. *)
type bin = {
  bin_name (*atd name *): bin_name;
  bin_sizes (*atd sizes *): (dimension * size) list;
  bin_cost (*atd cost *): bin_cost;
  bin_arity (*atd arity *): bin_arity
}

type binpacking_problem = {
  binpacking_problem_items (*atd items *): item list;
  binpacking_problem_bins (*atd bins *): bin list;
  binpacking_problem_incompatibilities (*atd incompatibilities *):
    (repository_name * incompatibilities) list
}
