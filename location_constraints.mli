
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val create_component_type_location_constraints   : configuration -> universe -> cstr list
val create_port_location_constraints             : configuration -> universe -> cstr list
val create_package_location_constraints          : configuration -> universe -> cstr list
val create_port_provided_at_location_constraints : configuration -> universe -> cstr list

val create_location_constraints : configuration -> universe -> cstr list