
open Aeolus_types_j
open Aeolus_types_output_facade
open Helpers


type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

let string_of_element element =
  match element with
  | ComponentType (component_type_name) -> Printf.sprintf "t %s" (string_of_component_type_name component_type_name)
  | Port          (port_name)           -> Printf.sprintf "p %s" (string_of_port_name           port_name)
  | Package       (package_name)        -> Printf.sprintf "k %s" (string_of_package_name        package_name)



type global_element_variable_key = element

let string_of_global_element_variable_key = 
  fun element ->
    Printf.sprintf
      "N(%s)"
      (string_of_element element)



type local_element_variable_key = location_name * element

let string_of_local_element_variable_key = 
  fun (location_name, element) ->
    Printf.sprintf 
      "N(%s,%s)"
      (string_of_location_name location_name)
      (string_of_element       element)



type binding_variable_key = port_name * component_type_name * component_type_name

let string_of_binding_variable_key = 
  fun (port_name, providing_component_type_name, requiring_component_type_name) ->
    Printf.sprintf 
      "B(%s,%s,%s)"
      (string_of_port_name           port_name)
      (string_of_component_type_name providing_component_type_name)
      (string_of_component_type_name requiring_component_type_name)



type local_repository_variable_key = location_name * repository_name

let string_of_local_repository_variable_key = 
  fun (location_name, repository_name) ->
    Printf.sprintf 
      "R(%s,%s)"
      (string_of_location_name   location_name)
      (string_of_repository_name repository_name)



type local_resource_variable_key = location_name * resource_name

let string_of_local_resource_variable_key = 
  fun (location_name, resource_name) ->
    Printf.sprintf 
      "O(%s,%s)"
      (string_of_location_name location_name)
      (string_of_resource_name resource_name)




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




let descr_of_variable_key variable_key =
  match variable_key with
  | GlobalElementVariable   (element)                                                                 -> "global element variable"
  | LocalElementVariable    (location_name, element)                                                  -> "local element variable"
  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) -> "binding variable"
  | LocalRepositoryVariable (location_name, repository_name)                                          -> "local repository variable"
  | LocalResourceVariable   (location_name, resource_name)                                            -> "local resource variable"


let string_of_variable_key variable_key =
  match variable_key with
  | GlobalElementVariable   (element) ->
      string_of_global_element_variable_key (element)

  | LocalElementVariable    (location_name, element) ->
      string_of_local_element_variable_key (location_name, element)

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      string_of_binding_variable_key (port_name, providing_component_type_name, requiring_component_type_name)

  | LocalRepositoryVariable (location_name, repository_name) ->
      string_of_local_repository_variable_key (location_name, repository_name)

  | LocalResourceVariable   (location_name, resource_name) ->
      string_of_local_resource_variable_key (location_name, resource_name)
