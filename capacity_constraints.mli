(** This module generates the constraints corresponding to initial (i.e. from the first draft)
  * typing context translation rules for the Aeolus flat model. *)

open Typing_context
open Variable_keys
open Generic_constraints


(** The constraint : [for each port p] P(p) = sum (over all (t,s) from the domain) of N(t,s) x delta((t,s),a,Provides) *)
(** Name           : Provide a given port. *)
(** Description    : Total number of provided ports p in the configuration is a sum of the quantity of port p provided by every resource present. *)
(** Explanation    : P(p) is equal to the total number of provided ports p. *)
val create_provide_constraints  : Typing_context.t -> cstr list


(** The constraint : [for each port p] P(p) >= sum (over all (t,s) from the domain) of N(t,s) x delta((t,s),a,Requires) *)
(** This constraint is obsolete! *)
val create_require_constraints  : Typing_context.t -> cstr list


(** The constraint : [for each resource type (t,s)] [for each port p which conflicts with resource type (t,s)] ( N(t,s) >= 1 )  implies  P(p) = delta((t,s),a,Provides) *)
(** Name           : Conflict of (t,s) on port p. *)
(** Description    : If a resource of type (t,s) is present in the configuration and it is in conflict with port p, then he can be the only resource providing port p present in the configuration. *)
(** Explanation    : If N(t,s) is greater than zero, then total number of ports p provided must be equal exactly the number that (t,s) provides. *)
val create_conflict_constraints : Typing_context.t -> cstr list
