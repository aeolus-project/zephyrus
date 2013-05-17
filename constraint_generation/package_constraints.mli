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

(** The constraint : Component types implemented by packages. *)
(** Description    : Each component must be implemented by a package. So on a location where there is at least one component of type t there must be installed at least one of the packages implementing component type t. *)
(** The constraint : [for each location l] [for each component type t]  ( N(l,t) >= 1 ) implies ( sum (over all packages k which can implement the component type t) N(l,k) >= 1 ) *)
(** Explanation    : If N(l,t) is greater than zero, then the sum of all packages that can implement component type t which are installed on the location l must be also greater than zero. *)
val create_package_implementation_constraints : configuration -> universe -> cstr list

(** The constraint : Package dependencies. *)
(** Description    : The package dependencies of every package installed on every location must be satisfied. Package dependencies are expressed as a set of sets of packages which should be regarded as a conjunction of disjunctions. If a package p is installed on a location, then from every on of these sets of packages at least one package must be installed on this location. *)
(** The constraint : [for each location l] [for each package k] [for each set g of packages that k depends on] N(l,k) <= sum (over all packages k' from the set g) N(l,k') *)
(** Explanation    : For each set of packages that package k depends on, N(l,k) must be smaller or equal to the number of packages from that set which are installed on location l. *)
val create_package_dependency_constraints     : configuration -> universe -> cstr list

(** The constraint : Package conflicts. *)
(** Description    : Two packages which are in conflict cannot be installed together on the same location. *)
(** The constraint : [for each location l] [for each package k1] [for each package k2 that k1 conflicts] N(l,k1) + N(l,k2) <= 1 *)
(** Explanation    : If package k1 conflicts package k2, then at most one of them can be installed on a given location. *)
val create_package_conflict_constraints       : configuration -> universe -> cstr list

val create_package_constraints : configuration -> universe -> cstr list