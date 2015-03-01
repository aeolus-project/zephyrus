(* Auto-generated from "coinst_conflicts.atd" *)


type id = Coinst_conflicts_t.id

type incompatibility = Coinst_conflicts_t.incompatibility

type class_definition = Coinst_conflicts_t.class_definition

type coinst_conflicts = Coinst_conflicts_t.coinst_conflicts = {
  classes: class_definition list;
  incompatibilities: incompatibility list
}

val validate_id :
  Ag_util.Validation.path -> id -> Ag_util.Validation.error option
  (** Validate a value of type {!id}. *)

val validate_incompatibility :
  Ag_util.Validation.path -> incompatibility -> Ag_util.Validation.error option
  (** Validate a value of type {!incompatibility}. *)

val validate_class_definition :
  Ag_util.Validation.path -> class_definition -> Ag_util.Validation.error option
  (** Validate a value of type {!class_definition}. *)

val create_coinst_conflicts :
  classes: class_definition list ->
  incompatibilities: incompatibility list ->
  unit -> coinst_conflicts
  (** Create a record of type {!coinst_conflicts}. *)

val validate_coinst_conflicts :
  Ag_util.Validation.path -> coinst_conflicts -> Ag_util.Validation.error option
  (** Validate a value of type {!coinst_conflicts}. *)

