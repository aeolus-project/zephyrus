
open Helpers
open Typing_context

type domain_variable_key = domain_element

let string_of_domain_variable_key domain_variable_key =
  Printf.sprintf "N%s" (string_of_domain_element domain_variable_key)

type port_variable_key = port_name

let string_of_port_variable_key port_variable_key =
  Printf.sprintf "P(%s)" (string_of_port_name port_variable_key)

type binding_variable_key = port_name * domain_element * domain_element 

let string_of_binding_variable_key =
  fun (port_name, providing_element, requiring_element) ->
  Printf.sprintf 
    "B(%s,%s,%s)"
    (string_of_port_name port_name)
    (string_of_domain_element providing_element)
    (string_of_domain_element requiring_element)

type max_rest_variables_group_name = T | MAX | REST

let string_of_max_rest_variables_group group_name =
  match group_name with
    T    -> "T"
  | MAX  -> "MAX"
  | REST -> "REST"

type max_rest_variable_key = max_rest_variables_group_name * port_name * int

let string_of_max_rest_variable_key =
  fun (max_rest_variables_group_name, port_name, i) ->
  Printf.sprintf 
    "%s(%s,%d)"
    (string_of_max_rest_variables_group max_rest_variables_group_name)
    (string_of_port_name port_name)
    i

(* TODO *)
type variable_key =
  | DomainVariable  of domain_element
  | PortVariable    of port_name
  | BindingVariable of port_name * domain_element * domain_element
  | MaxRestVariable of max_rest_variables_group_name * port_name * int

let descr_of_variable_key variable_key =
  match variable_key with
  | DomainVariable domain_variable_key                                -> "domain variable"
  | PortVariable port_variable_key                                    -> "port variable"
  | BindingVariable (port_name, providing_element, requiring_element) -> "port variable"
  | MaxRestVariable (max_rest_variables_group_name, port_name, i)     -> "max_rest variable"

let string_of_variable_key variable_key =
  match variable_key with
  | DomainVariable domain_variable_key -> 
      string_of_domain_variable_key domain_variable_key
  
  | PortVariable port_variable_key -> 
      string_of_port_variable_key port_variable_key
  
  | BindingVariable (port_name, providing_element, requiring_element) ->
      string_of_binding_variable_key (port_name, providing_element, requiring_element)

  | MaxRestVariable (max_rest_variables_group_name, port_name, i) -> 
      string_of_max_rest_variable_key (max_rest_variables_group_name, port_name, i)
