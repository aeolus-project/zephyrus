
open Typing_context


type domain_variable_key = domain_element

val string_of_domain_variable_key : domain_variable_key -> string



type port_variable_key = port_name

val string_of_port_variable_key : port_variable_key -> string



type binding_variable_key = port_name * domain_element * domain_element 

val string_of_binding_variable_key : binding_variable_key -> string



type max_rest_variables_group_name = T | MAX | REST

type max_rest_variable_key = max_rest_variables_group_name * port_name * int

val string_of_max_rest_variable_key : max_rest_variable_key -> string



type variable_key =
  | DomainVariable  of domain_element
  | PortVariable    of port_name
  | BindingVariable of port_name * domain_element * domain_element
  | MaxRestVariable of max_rest_variables_group_name * port_name * int

val descr_of_variable_key  : variable_key -> string
val string_of_variable_key : variable_key -> string
