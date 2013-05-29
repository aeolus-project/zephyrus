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


open Model_t
open Constraints

(** This module generates constraints encoding the proprieties of resources. *)

(** The constraint : Resources consumed by components and packages. *)
(** Description    : Resources consumed at a given location (by components and packages) cannot exceed the resources provided by that location. *)
(** The constraint : [for each location l] [for each resource o]  N(l,o) >= ( sum (over all component types t) resource_consumption(t,o) ) + ( sum (over all packages k) resource_consumption(k,o) ) *)
(** Explanation    : N(l,o) must be greater or equal to the sum of resource o consumed by all the components and packages which are installed at that location. *)
val create_local_resource_constraints                 : configuration -> universe -> cstr list

(** The constraint : Resources consumed by components. *)
(** Description    : Resources consumed at a given location (by components) cannot exceed the resources provided by that location. *)
(** The constraint : [for each location l] [for each resource o]  N(l,o) >= sum (over all component types t) resource_consumption(t,o) *)
(** Explanation    : N(l,o) must be greater or equal to the sum of resource o consumed by all the components which are installed at that location. *)
val create_local_component_resource_constraints       : configuration -> universe -> cstr list

(** The constraint : Resources provided by locations. *)
(** Description    : Each location provides a certain amount of each resource. *)
(** The constraint : [for each location l] [for each resource o] O(l,o) = resource_provide_arity(l,o) *)
(** Explanation    : O(l,o) is equal to amount of the resource o provided by the location l. *)
val create_initial_configuration_resource_constraints : configuration -> universe -> cstr list

val create_resource_constraints : configuration -> universe -> cstr list