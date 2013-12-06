(* Auto-generated from "json_versions.atd" *)


type version = Json_versions_t.version

type versioned_object = Json_versions_t.versioned_object = {
  version: version
}

val validate_version :
  Ag_util.Validation.path -> version -> Ag_util.Validation.error option
  (** Validate a value of type {!version}. *)

val create_versioned_object :
  ?version: version ->
  unit -> versioned_object
  (** Create a record of type {!versioned_object}. *)

val validate_versioned_object :
  Ag_util.Validation.path -> versioned_object -> Ag_util.Validation.error option
  (** Validate a value of type {!versioned_object}. *)

