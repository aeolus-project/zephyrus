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

(* Depends on
    - datatypes/Data_model (for names)
    - datatypes/Data_common (for sets and maps of variables)
*)

open Data_model

(* 1. Variables *)

type element = 
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

type variable = 
  | SpecificationVariable    of spec_variable_name
  (** Specifiaction variable *)

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

module Variable = struct t = variable let compare = Pervasives.compare end
module Variable_set = Data_common.Set.Make(Variable)
module Variable_map = Data_common.Map.Make(Variable)

(* 2. Constraints *)

type op = 
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)


type expression = 
  | Constant of int
  | Infty
  | Variable of variable
  | Reified  of t
  | Plus     of expression * expression (** Addition operator *)
  | Minus    of expression * expression (** Substraction operator *)
  | Times    of expression * expression (** Multiplication operator *)
and t = 
  | True              (** Always satisfied constraint. *)
  | Arith_constraint of expression * op * expression
  | And of t * t      (** And operator *)
  | Or of t * t       (** Or operator *)
  | Implies of t * t  (** Implies operator *)
  | Not of t          (** Not operator *)


(* 3. Solutions *)

type solution = int Variable_map.t (** solution of a constraint *)

