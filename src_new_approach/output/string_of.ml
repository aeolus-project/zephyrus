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
    - datatypes/Data_model
    - datatypes/Data_constraint
    - datatypes/Data_helper
*)

let identity = fun x -> x
let string_set s = "{" ^ (String.concat ", " (Data_common.SetString.elements s)) ^ "}"
let int_set s =  "{" ^ (String.concat ", " (List.map string_of_int (Data_common.SetInt.elements s))) ^ "}"

(************************************)
(** Model                           *)
(************************************)

(** 1. Resources *)
let resource_name     = identity
let resource_name_set = string_set
let resource_id       = string_of_int
let resource_id_set   = int_set

let resource_provide_arity r = string_of_int r
let resource_consume_arity r = string_of_int r

(** 2. Component types *)
let port_name     = identity
let port_name_set = string_set
let port_id       = string_of_int
let port_id_set   = int_set

let component_type_name     = identity
let component_type_name_set = string_set
let component_type_id       = string_of_int
let component_type_id_set   = int_set

let provide_arity arity = match arity with | Data_model.Infinite_provide -> "infinite" | Data_model.Finite_provide i -> string_of_int i
let require_arity       = string_of_int

(** 3. Packages *)
let package_name = identity
let package_id   = string_of_int

(** 4. Repositories *)
let repository_name r = r
let repository_id   r = string_of_int r

(** 5. Location *)
let location_name l = l
let location_id   l = string_of_int l

let component_name c = c
let component_id   c = string_of_int c

(** Specification *)
let spec_variable_name v = v
let spec_const const = string_of_int const

let spec_local_element e = match e with
  | Data_model.Spec_local_element_package (k) -> "#" ^ (package_id k)
  | Data_model.Spec_local_element_component_type (c) -> "#" ^ (component_type_id c)
  | Data_model.Spec_local_element_port (p) -> "#" ^ (port_id p)

let rec spec_local_expr e = match e with
  | Data_model.Spec_local_expr_var v -> spec_variable_name v
  | Data_model.Spec_local_expr_const c -> spec_const c
  | Data_model.Spec_local_expr_arity e -> spec_local_element e
  | Data_model.Spec_local_expr_add (e1, e2) -> "(" ^ (spec_local_expr e1) ^ " + " ^ (spec_local_expr e2) ^ ")"
  | Data_model.Spec_local_expr_sub (e1, e2) -> "(" ^ (spec_local_expr e1) ^ " - " ^ (spec_local_expr e2) ^ ")"
  | Data_model.Spec_local_expr_mul (e1, e2) -> (spec_const e1) ^ " * " ^ (spec_local_expr e2)

let spec_op o = match o with
  | Data_model.Lt  -> " < " | Data_model.LEq -> " <= " | Data_model.Eq  -> " = "
  | Data_model.GEq -> " >= " | Data_model.Gt  -> " > " | Data_model.NEq -> " <> "

let rec local_specification s = match s with
  | Data_model.Spec_local_true -> "true"
  | Data_model.Spec_local_op (e1, op, e2) -> "(" ^ (spec_local_expr e1) ^ (spec_op op) ^ (spec_local_expr e2) ^ ")"
  | Data_model.Spec_local_and (s1, s2) -> "(" ^ (local_specification s1) ^ " /\\ " ^ (local_specification s2) ^ ")"
  | Data_model.Spec_local_or (s1, s2) -> "(" ^ (local_specification s1) ^ " \\/ " ^ (local_specification s2) ^ ")"
  | Data_model.Spec_local_impl (s1, s2) -> "(" ^ (local_specification s1) ^ " => " ^ (local_specification s2) ^ ")"
  | Data_model.Spec_local_not (s') -> "not " ^ (local_specification s')

let spec_resource_constraint co = String.concat "; " (List.map (fun (o, op, i) -> (resource_id o) ^ (spec_op op) ^ (resource_provide_arity i)) co)
let spec_repository_constraint cr = match cr with | [] -> "_" | _ -> String.concat " \\/ " (List.map (fun r -> (repository_id r)) cr)

let spec_element e = match e with
  | Data_model.Spec_element_package (k) -> "#" ^ (package_id k)
  | Data_model.Spec_element_component_type (c) -> "#" ^ (component_type_id c)
  | Data_model.Spec_element_port (p) -> "#" ^ (port_id p)
  | Data_model.Spec_element_location (co, cr, ls) -> "#(" ^ (spec_resource_constraint co) ^ "){" ^ (spec_repository_constraint cr) ^ " : " ^ (local_specification ls) ^ "}"

let rec spec_expr e = match e with
  | Data_model.Spec_expr_var v -> spec_variable_name v
  | Data_model.Spec_expr_const c -> spec_const c
  | Data_model.Spec_expr_arity e -> spec_element e
  | Data_model.Spec_expr_add (e1, e2) -> "(" ^ (spec_expr e1) ^ " + " ^ (spec_expr e2) ^ ")"
  | Data_model.Spec_expr_sub (e1, e2) -> "(" ^ (spec_expr e1) ^ " - " ^ (spec_expr e2) ^ ")"
  | Data_model.Spec_expr_mul (e1, e2) -> (spec_const e1) ^ " * " ^ (spec_expr e2)

let rec specification s = match s with
  | Data_model.Spec_true -> "true"
  | Data_model.Spec_op (e1, op, e2) ->  (spec_expr e1) ^ (spec_op op) ^ (spec_expr  e2)
  | Data_model.Spec_and (s1, s2) -> "(" ^ (specification s1) ^ " /\\ " ^ (specification s2) ^ ")"
  | Data_model.Spec_or  (s1, s2) -> "(" ^ (specification s1) ^ " \\/ " ^ (specification s2) ^ ")"
  | Data_model.Spec_impl (s1, s2) -> "(" ^ (specification s1) ^ " => " ^ (specification s2) ^ ")"
  | Data_model.Spec_not (s') -> "not " ^ (specification s')


let rec model_optimization_function f = match f with
  | Data_model.Optimization_function_simple       -> "simple"
  | Data_model.Optimization_function_compact      -> "compact"
  | Data_model.Optimization_function_conservative -> "conservative"
  | Data_model.Optimization_function_spread       -> "spread"
  | Data_model.Optimization_function_none         -> "none"
 

(************************************)
(** Constraints                     *)
(************************************)


let element e = match e with
  | Data_constraint.Component_type(t) -> "Ntype(" ^ (component_type_id t) ^ ")"
  | Data_constraint.Port(p)           -> "Nport(" ^ (port_id p) ^ ")"
  | Data_constraint.Package(k)        -> "Npack(" ^ (package_id k) ^ ")"

let local_element l e = match e with
  | Data_constraint.Component_type(t) -> "Ntype(" ^ (location_id l) ^ ", " ^ (component_type_id t) ^ ")"
  | Data_constraint.Port(p)           -> "Nport(" ^ (location_id l) ^ ", " ^ (port_id p) ^ ")"
  | Data_constraint.Package(k)        -> "Npack(" ^ (location_id l) ^ ", " ^ (package_id k) ^ ")"

let variable v = match v with 
  | Data_constraint.Simple_variable(v)             -> spec_variable_name v
  | Data_constraint.Global_variable(e)             -> element e
  | Data_constraint.Local_variable(l,e)            -> local_element l e
  | Data_constraint.Binding_variable(p,t1,t2)      -> "B(" ^ (port_id p) ^ ", " ^ (component_type_id t1) ^ ", " ^ (component_type_id t2) ^ ")"
  | Data_constraint.Local_repository_variable(l,r) -> "R(" ^ (location_id l) ^ ", " ^ (repository_id r) ^ ")"
  | Data_constraint.Local_resource_variable(l,r)   -> "O(" ^ (location_id l) ^ ", " ^ (resource_id r) ^ ")"

let op o = match o with
  | Data_constraint.Lt  -> " < "
  | Data_constraint.LEq -> " <= "
  | Data_constraint.Eq  -> " = "
  | Data_constraint.GEq -> " >= "
  | Data_constraint.Gt  -> " > "
  | Data_constraint.NEq -> " <> "

let value v = match v with
  | Data_constraint.Finite_value(i) -> string_of_int i
  | Data_constraint.Infinite_value  -> "infinite"

let rec expression e = match e with
  | Data_constraint.Constant(v)  -> value v
  | Data_constraint.Variable(v)  -> variable v
  | Data_constraint.Reified(c)   -> "|| " ^ (konstraint c) ^ " ||"
  | Data_constraint.Add(l)       -> Data_helper.parse_nary_op "0" expression (fun s1 s2 -> s1 ^ " + " ^ s2) l
  | Data_constraint.Sub(e1,e2)   -> "(" ^ (expression e1) ^ " - " ^ (expression e2) ^ ")"
  | Data_constraint.Mul(l)       -> Data_helper.parse_nary_op "1" expression (fun s1 s2 -> s1 ^ " * " ^ s2) l
  | Data_constraint.Abs(e')      -> "|" ^ (expression e') ^ "|"
  | Data_constraint.Mod(e1,e2)   -> "(" ^ (expression e1) ^ " % " ^ (expression e2) ^ ")"
  | Data_constraint.Div(e1,e2)   -> "(" ^ (expression e1) ^ " / " ^ (expression e2) ^ ")"
and konstraint c = match c with
  | Data_constraint.True            -> "true"
  | Data_constraint.Arith(e1,o,e2)  -> (expression e1) ^ (op o) ^ (expression e2)
  | Data_constraint.And(l)          -> Data_helper.parse_nary_op "true" konstraint (fun s1 s2 -> s1 ^ " /\\ " ^ s2) l
  | Data_constraint.Or(l)           -> Data_helper.parse_nary_op "false" konstraint (fun s1 s2 -> s1 ^ " \\/ " ^ s2) l
  | Data_constraint.Implies(c1,c2)  -> (konstraint c1) ^ " ==> " ^ (konstraint c2)
  | Data_constraint.Not(c')         -> "not(" ^ (konstraint c') ^ ")"


