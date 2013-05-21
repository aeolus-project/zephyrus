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


open Aeolus_types_t
open Typing_context
open Variables
open Generic_constraints

(** The constraint : Resources consumed by component types and packages. *)
(** Description    : *)
(** The constraint : *)
(** Explanation    : *)
val create_local_resource_constraints                 : configuration -> universe -> cstr list

(** The constraint : Resources consumed by component types. *)
(** Description    : *)
(** The constraint : *)
(** Explanation    : *)
val create_local_component_resource_constraints       : configuration -> universe -> cstr list

(** The constraint : Resources provided by locations. *)
(** Description    : Each location provides a certain amount of each resource. *)
(** The constraint : [for each location l] [for each resource o] O(l,o) = resource_provide_arity(l,o) *)
(** Explanation    : O(l,o) is equal to amount of the resource o provided by the location l. *)
val create_initial_configuration_resource_constraints : configuration -> universe -> cstr list

val create_resource_constraints : configuration -> universe -> cstr list