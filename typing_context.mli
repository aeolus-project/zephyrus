
open Aeolus_types_j

val get_component_type                : universe -> component_type_name -> component_type
val get_component_type_implementation : universe -> component_type_name -> package_name list
val get_repository                    : universe -> repository_name -> repository
val get_package                       : repository -> package_name -> package

val get_component_type_names : universe -> component_type_name list
val get_port_names           : universe -> port_name list
val get_package_names        : universe -> package_name list
val get_repository_names     : universe -> repository_name list
val get_locations            : bare_architecture -> location_name list

val get_provide_arity : component_type -> port_name -> provide_arity
val get_require_arity : component_type -> port_name -> require_arity

val requirers   : universe -> port_name -> component_type_name list
val providers   : universe -> port_name -> component_type_name list
val conflicters : universe -> port_name -> component_type_name list

