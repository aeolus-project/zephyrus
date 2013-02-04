
open Helpers

module type RESOURCE_TYPES =
  sig
    type resource_type_name
    type state_name
    type port_name
    type port_quantity
    type state
    type resource_type
    type resource_types
  end

module RT = Resource_types_j

module type RESOURCE_TYPES_INPUT =
  sig
    val resource_type_name_of_string : string -> RT.resource_type_name
    val state_name_of_string         : string -> RT.state_name
    val port_name_of_string          : string -> RT.port_name
    val port_quantity_of_string      : string -> RT.port_quantity
    val state_of_string              : string -> RT.state
    val resource_type_of_string      : string -> RT.resource_type
    val resource_types_of_string     : string -> RT.resource_types
 end

module JSON_resource_types_input : RESOURCE_TYPES_INPUT = Resource_types_j
