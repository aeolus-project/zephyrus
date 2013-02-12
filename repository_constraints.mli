
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val create_one_repository_per_location_constraints                       : bare_architecture -> universe -> cstr list
val create_packages_in_location_must_come_from_its_repository_contraints : bare_architecture -> universe -> cstr list
val create_repository_constraints                                        : bare_architecture -> universe -> cstr list