(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(** This module generates the constraints corresponding to component types. *)

open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

(* The constraint : [for each component type t_r which requires port p]  require_arity(t_r,p) x N(t_r) = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) *)
(* Name           : Components of type t requiring port p. *)
(* Description    : All the require ports p of the components of type t must be bound. So the total number of ports p required by all the components of type t is equal to the total number of bindings providing these components with port p. *)
(* Constraint     : N(t) multiplied by number of ports p that each components of type t requires is equal to the sum of all bindings on port p where the component of type t is the requiring one and any other component is providing. *)
val create_require_binding_constraints : universe -> port_name -> cstr list


(* The constraint : [for each component type t_p which provides port p]  provide_arity(t_p,p) x N(t_p) = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p) *)
(* Name           : Components of type t providing port p. *)
(* Description    : There cannot exist more bindings providing port p from the components of type t than the total number of ports p they provide together. The total number of ports p provided by all the components of type t is equal or greater to the total number of bindings providing port p from these components. *)
(* Constraint     : N(t) multiplied by number of ports p that each component of type t provides is equal or greater to the sum of all bindings on port p where the component of type t is the providing one and any other component is the requiring one. *)
val create_provide_binding_constraints : universe -> port_name -> cstr list


(* The constraint : [for each component type t_p which provides port p] [for each component type t_r which requires port p]  B(t_p,t_r,p) <= N(t_p) x N(t_r) *)
(* Name           : Unicity of bindings between t_1 and t_2 on port p. *)
(* Description    : There cannot be more than one binding providing port p from a given component instance (of type t_1) to another given component instance (of type t_2). *)
(* Constraint     : The number of bindings where a component of type t_1 is providing a component of type t_2 with port p is not bigger than number of components of type t_1 multiplied by number of components of type t_2.*)
val create_binding_unicity_constraints : universe -> port_name -> cstr list


(* The constraint : [for each component type t which conflicts with port p]  ( N(t) >= 1 )  implies  ( N(p) = provides(t,p) ) *)
(* Name           : Conflict of t on port p. *)
(* Description    : If a component t which is in conflict with the port p is present in the configuration, then the total number of port p provided in the configuration must be equal to the number of port p which t provides. *)
(* Explanation    : If N(t) is greater than zero, then N(p) must be equal to the number of port p which t provides. *)
val create_conflict_constraints : universe -> port_name -> cstr list


(** Create all global component type constraints for a given universe. *)
val create_component_type_global_constraints : universe -> cstr list
