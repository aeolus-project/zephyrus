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

(** Component types, ports and packages are "elements". *)
(** What they have is common is that all three are a kind of objects in the configuration and 
    we often need to express their quantity in various contexts. Thanks to this type we can do it
    in a uniform fashion (for example: we have just a GlobalElementVariable representing the 
    quantity of a given element in the whole configuration, instead of having three different 
    variables: GlobalComponentTypeVariable, GlobalPortVariable and GlobalPackageVariable). *)
type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

(** Variables used in our constraints. *)
type variable =
  | GlobalElementVariable    of element
  (** Number of instances of a given component_type / port / package installed globally in the configuration. *)

  | LocalElementVariable     of location_name * element
  (** Number of instances of a given component_type / port / package installed on a given location. *)

  | BindingVariable          of port_name * component_type_name * component_type_name
  (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)

  | LocalRepositoryVariable  of location_name * repository_name
  (** Is the given repository installed on the given location? (boolean variable) *)

  | LocalResourceVariable    of location_name * resource_name
  (** How many resources of the given type are provided by the given location. *)

  | SpecificationVariable    of spec_variable_name
  (** Specifiaction variable *)


val string_of_variable : variable -> string
val descr_of_variable  : variable -> string


(** A hint for translating variables for solvers: what range of values can this variable have? *)
(** (It is implicitly introduces a constraint on the variable, so it is a little redundant, 
    but corresponds more directly to solver declarations.) *)
type variable_kind =
  | BooleanVariable (** The variable is representing a boolean value: 0 or 1. For example a repository can either be installed on a location or not, we cannot have two instances of a repository on the same location. *)
  | NaturalVariable (** The variable is representing a natural value: 0 to some max_int. *)

val variable_kind : variable -> variable_kind