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
  | Component_type of component_type_name
  | Port          of port_name
  | Package       of package_name

type variable = 
  | Simple_variable    of spec_variable_name  (** Specifiaction variable *)
  | Global_variable    of element             (** Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | Local_variable     of location_name * element (** Number of instances of a given component_type / port / package installed on a given location. *)
  | Binding_variable          of port_name * component_type_name * component_type_name
  (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | Local_repository_variable  of location_name * repository_name  (** Is the given repository installed on the given location? (boolean variable) *)
  | Local_resource_variable    of location_name * resource_name    (** How many resources of the given type are provided by the given location. *)

module Variable = struct type t = variable let compare = Pervasives.compare end
module Variable_set = Data_common.Set.Make(Variable)
module Variable_map = Data_common.Map.Make(Variable)

(* 2. Constraints *)

(* TODO: replace all And and Sum by operations on lists, to remove the need to add true and 0 everywhere *)

type op = 
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)

type value = provide_arity
let value_of_provide_arity = fun x -> x
let value_of_require_arity = fun x -> Finite_provide(x)
let value i = Finite_provide(i)
let infinite_value = Infinite_provide

type expression = 
  | Constant of value
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

(* 3. Optimization Function *)

type optimization_function = 
  | Minimize of expression
  | Maximize of expression
  | Lexicographic of optimization_function list

(* 4. Bounds *)

type bound = { min : value; max : value }    (** Each variable has a given search space in between [min] and [max]  *)
type variable_bound = variable -> bound  (** Function that gives for each variable its bound. Can be implemented with a map. *)


(* 5. Solutions *)

type solution = int Variable_map.t (** solution of a constraint *)

