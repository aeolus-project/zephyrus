(** This module generates the constraints corresponding to binding variables in 
  * the Aeolus flat model. *)

open Typing_context
open Variable_keys
open Generic_constraints


(** The constraint : [for each resource type (t_r,s_r) which requires port p]  delta((t_r,s_r),p,Requires) x N(t_r,s_r) = sum (over all (t_p,s_p) from the domain which provide port p) B((t_p,s_p),(t_r,s_r),p) *)
(** Name           : Resources of type (t,s) requiring port p. *)
(** Description    : All the require ports p of the resources of type (t,s) must be bound. So the total number of ports p required by all the resources of type (t,s) is equal to the total number of bindings providing these resources with port p. *)
(** Constraint     : N(t,s) multiplied by number of ports p that each resource of type (t,s) requires is equal to the sum of all bindings on port p where the resource of type (t,s) is the requiring one and any other resource is providing. *)
val create_require_binding_constraints : Typing_context.t -> port_name -> cstr list


(** The constraint : [for each resource type (t_p,s_p) which provides port p]  delta((t_p,s_p),p,Provides) x N(t_p,s_p) >= sum (over all (t_r,s_r) from the domain which require port p) B((t_p,s_p),(t_r,s_r),p) *)
(** Name           : Resources of type (t,s) providing port p. *)
(** Description    : There cannot exist more bindings providing port p from the resources of type (t,s) than the total number of ports p they provide together. The total number of ports p provided by all the resources of type (t,s) is equal or greater to the total number of bindings providing port p from these resources. *)
(** Constraint     : N(t,s) multiplied by number of ports p that each resource of type (t,s) provides is equal or greater to the sum of all bindings on port p where the resource of type (t,s) is the providing one and any other resource is the requiring one. *)
val create_provide_binding_constraints : Typing_context.t -> port_name -> cstr list


(** The constraint : [for each resource type (t_p,s_p) which provides port p] [for each resource type (t_r,s_r) which requires port p]  B((t_p,s_p),(t_r,s_r),p) <= N(t_p,s_p) x N(t_r,s_r) *)
(** Name           : Unicity of bindings between (t_p, s_p) and (t_r, s_r) on port p. *)
(** Description    : There cannot be more than one binding providing port p from a given resource instance (of type (t_p, s_p)) to another given resource instance (of type (t_r, s_r)). *)
(** Constraint     : The number of bindings where a resource of type (t_p, s_p) is providing a resource of type (t_r, s_r) with port p is not bigger than number of resources of type (t_p, s_p) multiplied by number of resources of type (t_r, s_r).*)
val create_unicity_binding_constraints : Typing_context.t -> port_name -> cstr list

(** Create all binding constraints for a given typing context. *)
val create_binding_constraints         : Typing_context.t -> cstr list