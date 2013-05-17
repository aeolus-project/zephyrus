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

(** The constraint : Component types by location. *)
(** Description    : For every component type t, if we sum up the local numbers of components of type t on all the locations, this sum must be equal to the global number of components of type t in the whole configuration. *)
(** The constraint : [for each component type t] N(t) = sum (over all locations l) N(l,t) *)
(** Explanation    : N(t) is equal to the sum of N(l,t) over all locations l. *)
val create_component_type_location_constraints   : configuration -> universe -> cstr list

(** The constraint : Ports by location. *)
(** Description    : For every port p, if we sum up the local numbers of ports p provided by components on all the locations, this sum must be equal to the global number of ports p provided by components in the whole configuration. *)
(** The constraint : [for each port p] N(p) = sum (over all locations l) N(l,p) *)
(** Explanation    : N(p) is equal to the sum of N(l,p) over all locations l. *)
val create_port_location_constraints             : configuration -> universe -> cstr list

(** The constraint : Packages by location. *)
(** Description    : For every package k, if we sum up the local numbers of packages k installed in all the locations, this sum must be equal to the global number of packages k in the whole configuration. *)
(** The constraint : [for each package k] N(k) = sum (over all locations l) N(l,k) *)
(** Explanation    : N(k) is equal to the sum of N(l,k) over all locations k. *)
val create_package_location_constraints          : configuration -> universe -> cstr list

(** The constraint : Ports provided on each location. *)
(** Description    : For every port p and location l, if we sum up the number of ports p provided by all the components on that location, this sum must be equal to the number of ports p provided by that location. *)
(** The constraint : [for each port p] [for each location l] N(l,p) = sum (over all component types t which provide port p) provide_arity(t,p) x N(l,t) = N(l,p) *)
(** Explanation    : N(l,p) is equal to the sum of all the ports p provided by components installed on the location l. *)
val create_port_provided_at_location_constraints : configuration -> universe -> cstr list

val create_location_constraints : configuration -> universe -> cstr list