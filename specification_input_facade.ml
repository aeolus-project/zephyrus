
open Helpers

module type SPECIFICATION_TYPES =
  sig
    type specification_constraint
    type specification_element
    type specification
  end

module S = Resource_types_j

module type SPECIFICATION_INPUT =
  sig
    val specification_constraint_of_string : string -> S.specification_constraint
    val specification_element_of_string    : string -> S.specification_element
    val specification_of_string            : string -> S.specification
  end

module JSON_specification_input : SPECIFICATION_INPUT = Resource_types_j
