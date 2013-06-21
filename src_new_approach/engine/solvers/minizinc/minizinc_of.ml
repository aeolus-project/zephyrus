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


open Data_constraint

type named_variables = string Variable_map.t
type structured_minizinc = { mzn_variables : named_variables; mzn_declaration : string; mzn_main_constraint : string; mzn_extra_constraint : string; mzn_output : string}

exception Wrong_optimization_function

(* 1. create names for the variables *)

  (* unsafe name creation *)
let sanitize_name name = Str.global_replace (Str.regexp "[a-z0-9]") "_" (String.lowercase name)
let name_of_t t = (sanitize_name (String_of.component_type_id t))
let name_of_p p = (sanitize_name (String_of.port_id p))
let name_of_k k = (sanitize_name (String_of.package_id k))
let name_of_l l = (sanitize_name (String_of.location_id l))
let name_of_element element = match element with
  | Component_type (t) -> "component_type_" ^ (name_of_t t)
  | Port           (p) -> "port_"           ^ (name_of_p p)
  | Package        (k) -> "package_"        ^ (name_of_l l)
let name_of_variable_unsafe v = match v with 
  | Simple_variable(v)             -> "spec_var_" ^ (sanitize_name (String_of.specification_variable v))
  | Global_variable(e)             -> "global_element_" ^ (name_of_element e)
  | Local_variable(l,e)            -> "local_element_" ^  ^ "_" ^ (name_of_element e)
  | Binding_variable(p,t1,t2)      -> "binding_" ^ (name_of_p p) ^ "_" ^ (name_of_t t1) ^ "_" ^ (name_of_t t2)
  | Local_repository_variable(l,r) -> "local_repository_" ^  ^ "_" ^ (sanitize_name (String_of.repository_id r))
  | Local_resource_variable(l,r)   -> "local_resource_" ^ (name_of_l l) ^ "_" ^ (sanitize_name (String_of.resource_id r))

  (* safe name creation *)
let name_var_id = ref 0
let name_of_variable_safe v = let id = !name_var_id in name_var_id := id + 1; "var_" ^ (string_of_int id)


(* 2. get all the variables in the constraint and optimization function *)

let rec variables_from_expression e = 
  | Constant(v)  -> Variable_set.empty
  | Variable(v)  -> Variable_set.singleton v
  | Reified(c)   -> variables_from_konstraint c
  | Add(l)       -> List.fold (fun e res -> Variable_set.union (variables_from_expression e) res) l Variable_set.empty
  | Sub(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e12)
  | Mul(l)       -> List.fold (fun e res -> Variable_set.union (variables_from_expression e) res) l Variable_set.empty
  | Abs(e')      -> variables_from_expression e'
  | Mod(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e12)
  | Div(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e12)
and variables_from_konstraint c = match c with
  | True            -> Variable_set.empty
  | Arith(e1,o,e2)  -> Variable_set.union (variables_from_expression e1) (variables_from_expression e12)
  | And(l)          -> List.fold (fun e res -> Variable_set.union (variables_from_konstraint e) res) l Variable_set.empty
  | Or(l)           -> List.fold (fun e res -> Variable_set.union (variables_from_konstraint e) res) l Variable_set.empty
  | Implies(c1,c2)  -> Variable_set.union (variables_from_konstraint e1) (variables_from_konstraint e12)
  | Not(c')         -> variables_from_konstraint e'

let rec variables_from_optimization_function f = match f with
  | Minimize (e) -> variables_from_expression e
  | Maximize (e) -> variables_from_expression e
  | Lexicographic (l) -> List.fold (fun f res -> Variable_set.union (variables_from_optimization_function f) res) l Variable_set.empty

let variables cs f =
  let vs = List.fold_left (fun res (_, c) -> Variable_set.union (variables_from_konstraint c) res) (variables_from_optimization_function f) cs in
  Variable_set.fold (fun v res -> Variable_map.add v (name_of_variable_unsafe v) res) vs Variable_map.empty


(* 3. variable declaration *)

let variable_declaration v_map f_bound =
  Variable_map.fold (fun v n res -> let (min,max) = f_bound v in res ^ ("var " ^ n ^ " : " ^ (Data_helper.int_of_value min)  ^ ".." ^ (Data_helper.int_of_value max) ^ ";\n")) 
    v_map ("var cost_var : 0.." ^ (string_of_int (Data_helper.int_of_value Data_constraint.Infinite_value)) ^ ";\n")

(* 4. core *)

let minizinc_of_op op = match op with
  | Lt  -> "<" | LEq -> "<=" | Eq  -> "=" | GEq -> ">=" | Gt  -> ">" | NEq -> "<>"

let rec minizinc_of_expression v_map e = match e with
  | Constant(v)  -> int_of_value v
  | Variable(v)  -> Variable_map.find v v_map
  | Reified(c)   -> "(bool2int " ^ (variables_from_konstraint v_map c) ^ ")"
  | Add(l)       -> "(" ^ (Data_helper.parse_nary_op  "0" (minizinc_of_expression v_map) (fun s1 s2 -> s1 ^ " + " ^ s2) l) ^ ")"
  | Sub(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " - " ^ (minizinc_of_expression v_map e2) ^ ")"
  | Mul(l)       -> "(" ^ (Data_helper.parse_nary_op "1" expression (fun s1 s2 -> s1 ^ " * " ^ s2) l) ^ ")"
  | Abs(e')      -> "(abs(" ^ (minizinc_of_expression v_map e') ^ "))"
  | Mod(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " % " ^ (minizinc_of_expression v_map e2) ^ ")"
  | Div(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " / " ^ (minizinc_of_expression v_map e2) ^ ")"
and minizinc_of_konstraint v_map c = match c with
  | True            -> "true"
  | Arith(e1,o,e2)  -> "(" ^ (minizinc_of_expression v_map e1) ^ (minizinc_of_op o) ^ (minizinc_of_expression v_map e2) ^ ")"
  | And(l)          -> "(" ^ (Data_helper.parse_nary_op "true" (minizinc_of_konstraint v_map) (fun s1 s2 -> s1 ^ " /\\ " ^ s2) l) ^ ")"
  | Or(l)           -> "(" ^ (Data_helper.parse_nary_op "false" (minizinc_of_konstraint v_map) (fun s1 s2 -> s1 ^ " \\/ " ^ s2) l) ^ ")"
  | Implies(c1,c2)  -> "(" ^ (minizinc_of_konstraint v_map c1) ^ " ==> " ^ (minizinc_of_konstraint v_map c2) ^ ")"
  | Not(c')         -> "(not(" ^ (minizinc_of_konstraint v_map c') ^ "))"

let minizinc_of_konstraints v_map cs =
  List.fold_left (fun res (s,c) -> ("%% " ^ s ^ "\n") ^ ("constraint " ^ (minizinc_of_konstraint v_map c) ^ ";") ^ res) "" cs

let output_of_variable v_map = 
  (Variable_map.fold (fun _ v res -> "  \"" ^ v ^ " = \", show(" ^ v ^ "), \";\\n\"") v_map "output [\\n  \"cost_var = \", show(cost_var), \";\\n\"") ^ "];"

let core v_map f_bound cs = {
  mzn_variables        = v_map;
  mzn_declaration      = variable_declaration v_map f_bound;
  mzn_main_constraint  = minizinc_of_konstraints v_map cs;
  mzn_extra_constraint = "";
  mzn_output           = output_of_variable v_map }

(* 5. extra constraint *)

let extra_constraint smzn e i = {
  mzn_variables        = smzn.mzn_variables;
  mzn_declaration      = smzn.mzn_declaration;
  mzn_main_constraint  = smzn.mzn_main_constraint;
  mzn_extra_constraint = smzn.mzn_extra_constraint ^ "\n" ^ (minizinc_of_expression smzn.mzn_variables e) ^ " = " ^ (string_of_int i);
  mzn_output           = smzn.mzn_output }


(* 6. finish *)

let rec goal_of_optimization_function v_map f = match f with
  | Maximize(e) -> "solve maximize (cost_var)"
  | Minimize(e) -> "solve minimize (cost_var)"
  | Lexicographic(l) -> match l with
    | [] -> "solve satisfy"
    | [f'] -> goal_of_optimization_function f'
    | _ -> raise Wrong_optimization_function

let optimization_goal smzn f = 
    "%% ======= MiniZinc file automatically generated by Zephyrus =======\n\n"
  ^ "%% === Variables ===\n" ^ smzn.mzn_declaration ^ "\n\n"
  ^ "%% === Main Constraints ===\n" ^ smzn.mzn_main_constraint ^ "\n\n"
  ^ "%% === Extra Constraints ===" ^ smzn.mzn_extra_constraint ^ "\n" ^ ("") ^ "\n\n"
  ^ "%% === Optimization function ===\n" ^ ("constraint cost_var = " ^ (minizinc_of_expression v_map e) ^ ";")
                                ^ (goal_of_optimization_function smzn.mzn_variables f) ^ "\n\n\n"
  ^ "%% === Output definition ===\n" ^ smzn.mzn_output ^ "\n" 




