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

type name = string
module Name_set = Data_common.SetString
module Name_map = Data_common.MapString

class type named_variables = object
  method variables : Variable_set.t
  method names     : Name_set.t
  method get_variable : string -> variable
  method get_name : variable -> string
end
type structured_minizinc = { mzn_variables : named_variables; mzn_declaration : string; mzn_main_constraint : string; mzn_extra_constraint : string; mzn_output : string}

exception Wrong_optimization_function

type minizinc = string


(* 1. create names for the variables *)

  (* unsafe name creation *)
let sanitize_name name = Str.global_replace (Str.regexp "[^a-z0-9]") "_" (String.lowercase name)
let name_of_t t = (sanitize_name (String_of.component_type_id t))
let name_of_p p = (sanitize_name (String_of.port_id p))
let name_of_k k = (sanitize_name (String_of.package_id k))
let name_of_l l = (sanitize_name (String_of.location_id l))
let name_of_element element = match element with
  | Component_type (t) -> "component_type_" ^ (name_of_t t)
  | Port           (p) -> "port_"           ^ (name_of_p p)
  | Package        (k) -> "package_"        ^ (name_of_k k)
let name_of_variable_unsafe v = match v with 
  | Simple_variable(v)             -> "spec_var_" ^ (sanitize_name (String_of.spec_variable_name v))
  | Global_variable(e)             -> "global_element_" ^ (name_of_element e)
  | Local_variable(l,e)            -> "local_element_" ^ (name_of_l l) ^ "_" ^ (name_of_element e)
  | Binding_variable(p,t1,t2)      -> "binding_" ^ (name_of_p p) ^ "_" ^ (name_of_t t1) ^ "_" ^ (name_of_t t2)
  | Local_repository_variable(l,r) -> "local_repository_" ^ (name_of_l l) ^ "_" ^ (sanitize_name (String_of.repository_id r))
  | Local_resource_variable(l,r)   -> "local_resource_" ^ (name_of_l l) ^ "_" ^ (sanitize_name (String_of.resource_id r))

  (* safe name creation *)
let name_var_id = ref 0
let name_of_variable_safe v = let id = !name_var_id in name_var_id := id + 1; "var_" ^ (string_of_int id)


(* 2. get all the variables in the constraint and optimization function *)

let rec variables_from_expression e = match e with
  | Constant(v)  -> Variable_set.empty
  | Variable(v)  -> Variable_set.singleton v
  | Reified(c)   -> variables_from_konstraint c
  | Add(l)       -> List.fold_left (fun res e -> Variable_set.union (variables_from_expression e) res) Variable_set.empty l
  | Sub(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e2)
  | Mul(l)       -> List.fold_left (fun res e -> Variable_set.union (variables_from_expression e) res) Variable_set.empty l
  | Abs(e')      -> variables_from_expression e'
  | Mod(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e2)
  | Div(e1,e2)   -> Variable_set.union (variables_from_expression e1) (variables_from_expression e2)
and variables_from_konstraint c = match c with
  | True            -> Variable_set.empty
  | Arith(e1,o,e2)  -> Variable_set.union (variables_from_expression e1) (variables_from_expression e2)
  | And(l)          -> List.fold_left (fun res e -> Variable_set.union (variables_from_konstraint e) res) Variable_set.empty l
  | Or(l)           -> List.fold_left (fun res e -> Variable_set.union (variables_from_konstraint e) res) Variable_set.empty l
  | Implies(c1,c2)  -> Variable_set.union (variables_from_konstraint c1) (variables_from_konstraint c2)
  | Not(c')         -> variables_from_konstraint c'

let rec variables_from_optimization_function f = match f with
  | Minimize (e) -> variables_from_expression e
  | Maximize (e) -> variables_from_expression e
  | Lexicographic (l) -> List.fold_left (fun res f -> Variable_set.union (variables_from_optimization_function f) res) Variable_set.empty l

let get_named_variables cs f =
  let vs = List.fold_left (fun res (_, c) -> Variable_set.union (variables_from_konstraint c) res) (variables_from_optimization_function f) cs in
  let variables_tmp = ref Variable_set.empty in
  let names_tmp = ref Name_set.empty in
  let get_name_tmp = ref Variable_map.empty in
  let get_variable_tmp = ref Name_map.empty in
  let add v n = 
    variables_tmp := Variable_set.add v !variables_tmp;
    names_tmp := Name_set.add n !names_tmp;
    get_name_tmp := Variable_map.add v n !get_name_tmp;
    get_variable_tmp := Name_map.add n v !get_variable_tmp in
  Variable_set.iter (fun v -> add v (name_of_variable_unsafe v)) vs; object (* <- here, we can switch to safe name creation *)
    method variables = !variables_tmp
    method names     = !names_tmp
    method get_variable n = Name_map.find n !get_variable_tmp
    method get_name     v = Variable_map.find v !get_name_tmp
  end


(* 3. variable declaration *)
let cost_variable_name = "cost_var"

let variable_declaration v_map f_bound =
  Variable_set.fold (fun v res -> let bound = f_bound v in
      let (min, max) = (string_of_int (Data_helper.int_of_value bound.min), string_of_int (Data_helper.int_of_value bound.max)) in
      res ^ ("var " ^ min  ^ ".." ^ max ^ " : "  ^ (v_map#get_name v) ^ ";\n")) 
    v_map#variables ("var 0.." ^ (string_of_int (Data_helper.int_of_value Data_constraint.Infinite_value)) ^ " : " ^ cost_variable_name ^ ";\n")

(* 4. core *)

let minizinc_of_op op = match op with
  | Lt  -> " < " | LEq -> " <= " | Eq  -> " = " | GEq -> " >= " | Gt  -> " > " | NEq -> " != "

let rec minizinc_of_expression v_map e = match e with
  | Constant(v)  -> (string_of_int (Data_helper.int_of_value v))
  | Variable(v)  -> v_map#get_name v
  | Reified(c)   -> "(bool2int " ^ (minizinc_of_konstraint v_map c) ^ ")"
  | Add(l)       -> "(" ^ (Data_helper.parse_nary_op  "0" (minizinc_of_expression v_map) (fun s1 s2 -> s1 ^ " + " ^ s2) l) ^ ")"
  | Sub(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " - " ^ (minizinc_of_expression v_map e2) ^ ")"
  | Mul(l)       -> "(" ^ (Data_helper.parse_nary_op "1" (minizinc_of_expression v_map) (fun s1 s2 -> s1 ^ " * " ^ s2) l) ^ ")"
  | Abs(e')      -> "(abs(" ^ (minizinc_of_expression v_map e') ^ "))"
  | Mod(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " % " ^ (minizinc_of_expression v_map e2) ^ ")"
  | Div(e1,e2)   -> "(" ^ (minizinc_of_expression v_map e1) ^ " / " ^ (minizinc_of_expression v_map e2) ^ ")"
and minizinc_of_konstraint v_map c = match c with
  | True            -> "true"
  | Arith(e1,o,e2)  -> "(" ^ (minizinc_of_expression v_map e1) ^ (minizinc_of_op o) ^ (minizinc_of_expression v_map e2) ^ ")"
  | And(l)          -> "(" ^ (Data_helper.parse_nary_op "true" (minizinc_of_konstraint v_map) (fun s1 s2 -> s1 ^ " /\\ " ^ s2) l) ^ ")"
  | Or(l)           -> "(" ^ (Data_helper.parse_nary_op "false" (minizinc_of_konstraint v_map) (fun s1 s2 -> s1 ^ " \\/ " ^ s2) l) ^ ")"
  | Implies(c1,c2)  -> "(" ^ (minizinc_of_konstraint v_map c1) ^ " -> " ^ (minizinc_of_konstraint v_map c2) ^ ")"
  | Not(c')         -> "(not(" ^ (minizinc_of_konstraint v_map c') ^ "))"

let minizinc_of_konstraints v_map cs =
  List.fold_left (fun res (s,c) -> ("\n%% " ^ s ^ "\n") ^ ("constraint " ^ (minizinc_of_konstraint v_map c) ^ ";") ^ res) "" cs

let output_of_variables v_map = 
  let output_of_variable v = "  \"" ^ v ^ " = \", show(" ^ v ^ "), \";\\n\"" in
  "output [\n" ^ (String.concat ",\n" (List.map output_of_variable (cost_variable_name::(Name_set.elements v_map#names)))) ^ "\n];\n\n"

let core_translation v_map f_bound cs = {
  mzn_variables        = v_map;
  mzn_declaration      = variable_declaration v_map f_bound;
  mzn_main_constraint  = minizinc_of_konstraints v_map cs;
  mzn_extra_constraint = "";
  mzn_output           = output_of_variables v_map }

(* 5. extra constraint *)

let add_extra_constraint smzn e i = {
  mzn_variables        = smzn.mzn_variables;
  mzn_declaration      = smzn.mzn_declaration;
  mzn_main_constraint  = smzn.mzn_main_constraint;
  mzn_extra_constraint = smzn.mzn_extra_constraint ^ "\nconstraint (" ^ (minizinc_of_expression smzn.mzn_variables e) ^ " = " ^ (string_of_int i) ^ ");";
  mzn_output           = smzn.mzn_output }


(* 6. finish *)

let rec goal_of_optimization_function v_map f = match f with
  | Maximize(e) -> (Some(e), "solve maximize (" ^ cost_variable_name ^ ");")
  | Minimize(e) -> (Some(e), "solve minimize (" ^ cost_variable_name ^ ");")
  | Lexicographic(l) -> match l with
    | [] -> (None, "solve satisfy;")
    | [f'] -> goal_of_optimization_function v_map f'
    | _ -> raise Wrong_optimization_function

let add_optimization_goal smzn f = let (eo,s) = goal_of_optimization_function smzn.mzn_variables f in
    "%% ======= MiniZinc file automatically generated by Zephyrus =======\n\n"
  ^ "%% === Variables ===\n" ^ smzn.mzn_declaration ^ "\n\n"
  ^ "%% === Main Constraints ===\n" ^ smzn.mzn_main_constraint ^ "\n\n"
  ^ "%% === Extra Constraints ===" ^ smzn.mzn_extra_constraint ^ "\n" ^ ("") ^ "\n\n"
  ^ "%% === Optimization function ===\n"
    ^ (match eo with None -> "" | Some(e) ->"constraint " ^ cost_variable_name ^ " = " ^ (minizinc_of_expression smzn.mzn_variables e) ^ ";\n\n") ^ s ^ "\n\n\n"
  ^ "%% === Output definition ===\n" ^ smzn.mzn_output ^ "\n" 




