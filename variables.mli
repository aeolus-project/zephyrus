
open Aeolus_types_j
open Variable_keys

type variable_kind =
  | BooleanVariable
  | NaturalVariable

val variable_kind_of_variable_key : variable_key -> variable_kind

val get_variable_keys : universe -> configuration -> specification -> variable_key list