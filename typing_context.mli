
open Resource_types_t


(** Type port_name *)
type port_name = Resource_types_t.port_name

val string_of_port_name           : port_name -> string


(** Type port_relation_type *)
type port_relation_type = Provides | Requires

val string_of_port_relation_type  : port_relation_type -> string


(** Type domain_element *)
type domain_element = (resource_type_name * state_name)

val string_of_domain_element      : domain_element -> string


(** Type typing_context *)
type t

(** Creating *)
val create_typing_context : resource_types -> t

(** Accessing *)
val ports  : t -> port_name list
val domain : t -> domain_element list
val delta  : t -> domain_element -> port_name -> port_relation_type -> port_quantity
val gamma  : t -> domain_element -> port_name list

(** Printing *)
val string_of_domain  : t -> string
val string_of_ports   : t -> string
val string_of_delta   : t -> port_relation_type -> string
val string_of_gamma   : t -> string


val requires : t -> domain_element -> (port_name * int) list
val provides : t -> domain_element -> (port_name * int) list

(* Functions for Max-Rest constraints *)

val r           : t -> port_name -> int option
val max_provide : t -> port_name -> int

val string_of_r            : t -> string
val string_of_max_provide  : t -> string
