
open Helpers

module type SPECIFICATION_TYPES =
  sig
    type spec_variable_name
    type spec_const
    type specification
    type spec_expr
    type spec_element
    type local_specification
    type spec_local_expr
    type spec_local_element
    type spec_resource_constraint
    type spec_resource_constraints
    type spec_repository_constraint
    type spec_repository_constraints
    type spec_op
  end

module S = Aeolus_types_j

module type SPECIFICATION_INPUT =
  sig
    val spec_variable_name_of_string          : string -> S.spec_variable_name
    val spec_const_of_string                  : string -> S.spec_const
    val specification_of_string               : string -> S.specification
    val spec_expr_of_string                   : string -> S.spec_expr
    val spec_element_of_string                : string -> S.spec_element
    val local_specification_of_string         : string -> S.local_specification
    val spec_local_expr_of_string             : string -> S.spec_local_expr
    val spec_local_element_of_string          : string -> S.spec_local_element
    val spec_resource_constraint_of_string    : string -> S.spec_resource_constraint
    val spec_resource_constraints_of_string   : string -> S.spec_resource_constraints
    val spec_repository_constraint_of_string  : string -> S.spec_repository_constraint
    val spec_repository_constraints_of_string : string -> S.spec_repository_constraints
    val spec_op_of_string                     : string -> S.spec_op
  end

module JSON_specification_input : SPECIFICATION_INPUT = Aeolus_types_j
