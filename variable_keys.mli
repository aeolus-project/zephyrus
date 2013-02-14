
open Aeolus_types_t

type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

val string_of_element : element -> string

type global_element_variable_key = element
val string_of_global_element_variable_key : global_element_variable_key -> string

type local_element_variable_key = location_name * element
val string_of_local_element_variable_key : local_element_variable_key -> string

type binding_variable_key = port_name * component_type_name * component_type_name
val string_of_binding_variable_key : binding_variable_key -> string

type local_repository_variable_key = location_name * repository_name
val string_of_local_repository_variable_key : local_repository_variable_key -> string

type local_resource_variable_key = location_name * resource_name
val string_of_local_resource_variable_key : local_resource_variable_key -> string

type specification_variable_key = spec_variable_name
val string_of_specification_variable_key : specification_variable_key -> string

type variable_key =
  (* Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | GlobalElementVariable    of element

  (* Number of instances of a given component_type / port / package installed on a given location. *)
  | LocalElementVariable     of location_name * element

  (* Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | BindingVariable          of port_name * component_type_name * component_type_name

  (* Is the given repository installed on the given location? (boolean variable) *)
  | LocalRepositoryVariable  of location_name * repository_name

  (* How many resources of the given type are provided by the given location. *)
  | LocalResourceVariable    of location_name * resource_name

  (* Specifiaction variable *)
  | SpecificationVariable    of spec_variable_name


val descr_of_variable_key  : variable_key -> string
val string_of_variable_key : variable_key -> string

(* Predicates on variable types *)
(* TODO: This is mostly boilerplate code, but I have no idea how to get rid of it... *)
val pred_global_element_variable   : variable_key -> bool
val pred_local_element_variable    : variable_key -> bool
val pred_binding_variable          : variable_key -> bool
val pred_local_repository_variable : variable_key -> bool
val pred_local_resource_variable   : variable_key -> bool
val pred_specification_variable    : variable_key -> bool