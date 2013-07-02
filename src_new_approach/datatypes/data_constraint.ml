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
  | Component_type of component_type_id
  | Port           of port_id
  | Package        of package_id

type variable = 
  | Simple_variable            of spec_variable_name           (** Specifiaction variable *)
  | Global_variable            of element                      (** Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | Local_variable             of location_id * element        (** Number of instances of a given component_type / port / package installed on a given location. *)
  | Binding_variable           of port_id * component_type_id * component_type_id
    (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | Local_repository_variable  of location_id * repository_id  (** Is the given repository installed on the given location? (boolean variable) *)
  | Local_resource_variable    of location_id * resource_id    (** How many resources of the given type are provided by the given location. *)

module Variable = struct type t = variable let compare = Pervasives.compare end
module Variable_set = Data_common.Set.Make(Variable)
module Variable_map = Data_common.Map.Make(Variable)

(* 2. Values *)

type value = Finite_value of int | Infinite_value

module Value = struct
  type t = value
  
  let of_int n = Finite_value n
  let infty    = Infinite_value
  
  let value_of_provide_arity p = match p with | Finite_provide(n) -> of_int n | Infinite_provide -> infty
  let value_of_require_arity r = of_int r

  let min v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (min n1 n2)
    | (Finite_value _ , _              ) -> v1
    | (_              , Finite_value _ ) -> v2
    | _  -> Infinite_value

  let max v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (max n1 n2)
    | _ -> Infinite_value

  let sum v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 + n2)
    | _ -> Infinite_value

  let prod v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 * n2)
    | _ -> Infinite_value
  
  let div v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 / n2)
    | (Finite_value _ , _              ) -> of_int 0
    | (_              , Finite_value _ ) -> infty
    | _  -> of_int 1
  
end

(* 3. Constraints *)

(* Remark: all associative and commutative operators have lists in arguement, for more efficient encoding *)

type op = 
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)


type expression = 
  | Constant of value
  | Variable of variable
  | Reified  of konstraint
  | Add      of expression list         (** Addition operator *)
  | Sub      of expression * expression (** Substraction operator *)
  | Mul      of expression list         (** Multiplication operator *)
  | Abs      of expression
  | Mod      of expression * expression
  | Div      of expression * expression
and konstraint = 
  | True                                  (** Always satisfied constraint. *)
  | Arith of expression * op * expression
  | And of konstraint list                (** And operator *)
  | Or of konstraint list                 (** Or operator *)
  | Implies of konstraint * konstraint    (** Implies operator *)
  | Not of konstraint                     (** Not operator *)

(* 3. Optimization Function *)

type optimization_function = 
  | Minimize of expression
  | Maximize of expression
  | Lexicographic of optimization_function list

(* 4. Bounds *)

type bound = { min : value; max : value }    (** Each variable has a given search space in between [min] and [max]  *)
type variable_bounds = variable -> bound  (** Function that gives for each variable its bound. Can be implemented with a map. *)


(* 5. Solutions *)

type solution = {
  domain          : Variable_set.t;
  variable_values : variable -> int; (** solution of a constraint, can be implemented with a [Variable_map.t] *)
}

