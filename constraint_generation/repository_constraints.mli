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

(** The constraint : One repository on each location. *)
(** Description    : On each location there must be exactly one repository installed. *)
(** The constraint : [for each location l] ( sum (over all repositories r) R(l,r) ) = 1 *)
(** Explanation    : The sum of all the repositories r installed on a location l must be equal exactly one. *)
val create_one_repository_per_location_constraints                       : configuration -> universe -> cstr list

(** The constraint : Repository and installed packages. *)
(** Description    : All the packages installed on a location must come from the repository installed on that location. *)
(** The constraint : [for each location l] [for each repository r] [for each package k] ( R(l,r) = 1 )  implies  (if k belongs to r then ( N(l,k) <= 1 ) if it does not then ( N(l,k) = 0 ) *)
(** Explanation    : If R(l,r) is equal to one, then for all the packages k which belong to r, N(l,k) must be smaller or equal to one and for all the packages k which do not belong to r, N(l,k) must be equal to zero. *)
val create_packages_in_location_must_come_from_its_repository_contraints : configuration -> universe -> cstr list

val create_repository_constraints : configuration -> universe -> cstr list