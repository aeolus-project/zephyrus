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
let string_list s = "{" ^ (String.concat ", " s) ^ "}"
let int_list    s = string_list (List.map string_of_int s)

let string_set s = string_list (Data_common.SetString.elements s)
let string_set_set s = "[ " ^ (String.concat "; " (List.map string_set (Data_common.SetSetString.elements s))) ^ " ]"
let int_set s =  int_list (Data_common.SetInt.elements s)
let int_set_set s = "[ " ^ (String.concat "; " (List.map int_set (Data_common.SetSetInt.elements s))) ^ " ]"


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
let package_name     = identity
let package_name_set = string_set
let package_id       = string_of_int
let package_id_set   = int_set

(** 4. Repositories *)
let repository_name   r = r
let repository_name_set = string_set
let repository_id     r = string_of_int r
let repository_id_set   = int_set

(** 5. Location *)
let location_name l   = l
let location_name_set = string_set
let location_id   l   = string_of_int l
let location_id_set   = int_set

let location_categories = int_set_set

let component_name c   = c
let component_name_set = string_set
let component_id   c   = string_of_int c
let component_id_set   = int_set

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
  | Data_constraint.Location_used_variable(l)      -> "U(" ^ (location_id l) ^ ")"


let unary_arith_op = function
  | Data_constraint.Abs -> "abs"

let binary_arith_op = function
  | Data_constraint.Add -> "+"
  | Data_constraint.Sub -> "-"
  | Data_constraint.Mul -> "*"
  | Data_constraint.Div -> "/"
  | Data_constraint.Mod -> "%"

let nary_arith_op = function
  | Data_constraint.Sum     -> "+"
  | Data_constraint.Product -> "*"

let unit_of_nary_arith_op = function
  | Data_constraint.Sum     -> "0"
  | Data_constraint.Product -> "1"

let arith_cmp_op = function
  | Data_constraint.Lt  -> "<"
  | Data_constraint.LEq -> "<="
  | Data_constraint.Eq  -> "="
  | Data_constraint.GEq -> ">="
  | Data_constraint.Gt  -> ">"
  | Data_constraint.NEq -> "<>"

let unary_konstraint_op = function
  | Data_constraint.Not -> "not"

let binary_konstraint_op = function
  | Data_constraint.Implies -> "=>"

let nary_konstraint_op = function
  | Data_constraint.And -> "/\\"
  | Data_constraint.Or  -> "\\/"

let unit_of_nary_konstraint_op = function
  | Data_constraint.And -> "true"
  | Data_constraint.Or  -> "false"


let value v = match v with
  | Data_constraint.Finite_value(i) -> string_of_int i
  | Data_constraint.Infinite_value  -> "infinite"

let rec expression e = match e with
  | Data_constraint.Constant              (v)        -> value v
  | Data_constraint.Variable              (v)        -> variable v
  | Data_constraint.Reified               (c)        -> Printf.sprintf "|| %s ||" (konstraint c)
  | Data_constraint.UnaryArithExpression  (op,e)     -> Printf.sprintf "%s (%s)" (unary_arith_op op) (expression e)
  | Data_constraint.BinaryArithExpression (op,e1,e2) -> Printf.sprintf "(%s %s %s)" (expression e1) (binary_arith_op op) (expression e2)
  | Data_constraint.NaryArithExpression   (op,l)     -> Printf.sprintf "(%s)"
                                                        (if l = [] 
                                                         then unit_of_nary_arith_op op
                                                         else String.concat (" " ^ (nary_arith_op op) ^ " ") (List.map expression l) )

and konstraint c = match c with
  | Data_constraint.True                        -> "true"
  | Data_constraint.False                       -> "false"
  | Data_constraint.ArithKonstraint  (op,e1,e2) -> Printf.sprintf "(%s %s %s)" (expression e1) (arith_cmp_op op) (expression e2)
  | Data_constraint.UnaryKonstraint  (op,c)     -> Printf.sprintf "%s (%s)" (unary_konstraint_op op) (konstraint c)
  | Data_constraint.BinaryKonstraint (op,c1,c2) -> Printf.sprintf "(%s %s %s)" (konstraint c1) (binary_konstraint_op op) (konstraint c2)
  | Data_constraint.NaryKonstraint   (op,l)     -> Printf.sprintf "(%s)"
                                                   (if l = []
                                                    then unit_of_nary_konstraint_op op
                                                    else String.concat (" " ^ (nary_konstraint_op op) ^ " ") (List.map konstraint l) )


let described_konstraint (c,k) = c ^ (konstraint k) ^ "\n"
let described_konstraint_list l = List.fold_left (fun res el -> (described_konstraint el) ^ res) "" l

let constraint_optimization_function f =
  let rec inner accu f = match f with
  | Data_constraint.Minimize e -> "Minimize " ^  (expression e)
  | Data_constraint.Maximize e -> "Maximize " ^  (expression e)
  | Data_constraint.Lexicographic l -> String.concat ("\n" ^ accu ^ "then ") (List.map (inner (accu ^ "  ")) l) in inner "  " f

(************************************)
(** Solution                        *)
(************************************)

let solution solution =
  let open Data_constraint in
  let variables = Variable_set.elements solution.domain in
  let strings = List.map (fun var -> 
    Printf.sprintf "%-15s = %d\n" (variable var) (solution.variable_values var)
    ) variables in
  String.concat "" strings



(************************************)
(** Configuration                   *)
(************************************)

let configuration universe configuration =
  let open Data_model in
  
  Printf.printf "\n%s\n" (int_set universe#get_repository_ids);

  let location_ids = Location_id_set.elements configuration#get_location_ids in
  
  let location_strings = List.map (fun lid ->

    let l = configuration#get_location lid in
    assert(configuration#get_location_id l#name = lid);
    assert(configuration#get_location_name lid = l#name);
    let line1 =Printf.sprintf "%s -> %s\n" (location_id lid) (location_name l#name) in

    let rid = l#repository in
    let r = universe#get_repository rid in
    assert(universe#get_repository_id r#name = rid);
    assert(universe#get_repository_name rid = r#name);
    let line2 = Printf.sprintf "    + repository : %s -> %s\n" (repository_id rid) (repository_name r#name) in

    let kids = l#packages_installed in
    let module Package_set_of_package_id_set = Data_common.Set.Convert(Package_id_set)(Package_set) in
    let ks = Package_set_of_package_id_set.convert universe#get_package kids in
    let module Package_name_set_of_package_set = Data_common.Set.Convert(Package_set)(Package_name_set) in
    let knames = Package_name_set_of_package_set.convert Data_common.get_name ks in
    let line3 = Printf.sprintf "    + packages : %s\n" (string_set knames) in

    Printf.sprintf "%s%s%s" line1 line2 line3
  ) location_ids in

  String.concat "" location_strings
