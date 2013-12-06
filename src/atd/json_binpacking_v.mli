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

val validate_resource_name :
  Ag_util.Validation.path -> resource_name -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_name}. *)

val validate_resource_consume_arity :
  Ag_util.Validation.path -> resource_consume_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_consume_arity}. *)

val validate_resource_provide_arity :
  Ag_util.Validation.path -> resource_provide_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_provide_arity}. *)

val validate_item_name :
  Ag_util.Validation.path -> item_name -> Ag_util.Validation.error option
  (** Validate a value of type {!item_name}. *)

val validate_item_arity :
  Ag_util.Validation.path -> item_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!item_arity}. *)

val create_item :
  item_name: item_name ->
  item_consume: (resource_name * resource_consume_arity) list ->
  item_arity: item_arity ->
  unit -> item
  (** Create a record of type {!item}. *)

val validate_item :
  Ag_util.Validation.path -> item -> Ag_util.Validation.error option
  (** Validate a value of type {!item}. *)

val validate_bin_name :
  Ag_util.Validation.path -> bin_name -> Ag_util.Validation.error option
  (** Validate a value of type {!bin_name}. *)

val validate_bin_cost :
  Ag_util.Validation.path -> bin_cost -> Ag_util.Validation.error option
  (** Validate a value of type {!bin_cost}. *)

val validate_bin_arity :
  Ag_util.Validation.path -> bin_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!bin_arity}. *)

val create_bin :
  bin_name: bin_name ->
  bin_provide: (resource_name * resource_provide_arity) list ->
  bin_cost: bin_cost ->
  bin_arity: bin_arity ->
  unit -> bin
  (** Create a record of type {!bin}. *)

val validate_bin :
  Ag_util.Validation.path -> bin -> Ag_util.Validation.error option
  (** Validate a value of type {!bin}. *)

val create_binpacking_problem :
  binpacking_problem_items: item list ->
  binpacking_problem_bins: bin list ->
  unit -> binpacking_problem
  (** Create a record of type {!binpacking_problem}. *)

val validate_binpacking_problem :
  Ag_util.Validation.path -> binpacking_problem -> Ag_util.Validation.error option
  (** Validate a value of type {!binpacking_problem}. *)

