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
module Name_set = Data_common.String_set
module Name_map = Data_common.String_map

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

(* TODO: put get variables somewhere else, and use a catalog. This algorithm is really unsafe *)

  (* unsafe name creation *)
let my_regexp = Str.regexp "[^a-z0-9]"
(* TODO: This is a dirty hack, we should just handle deprecated stuff better. *)
let handle_deprecated string_of_f id = if id = -1 then "deprecated" else string_of_f id
let sanitize_name name = Str.global_replace my_regexp "_" (String.lowercase name)
let name_of_t t = (sanitize_name (handle_deprecated Name_of.component_type_id t))
let name_of_p p = (sanitize_name (handle_deprecated Name_of.port_id p))
let name_of_k k = (sanitize_name (handle_deprecated Name_of.package_id k))
let name_of_l l = (sanitize_name (handle_deprecated Name_of.location_id l))
let name_of_r r = (sanitize_name (handle_deprecated Name_of.repository_id r))
let name_of_o o = (sanitize_name (handle_deprecated Name_of.resource_id o))
let name_of_element element = match element with
  | Component_type (t) -> "component_" ^ (name_of_t t)
  | Port           (p) -> "port_"           ^ (name_of_p p)
  | Package        (k) -> "package_"        ^ (name_of_k k)
let name_of_variable_unsafe v = match v with 
  | Simple_variable(v)             -> "spec_var_" ^ (sanitize_name (String_of.spec_variable_name v))
  | Global_variable(e)             -> "global_" ^ (name_of_element e)
  | Local_variable(l,e)            -> "local_" ^ (name_of_l l) ^ "_" ^ (name_of_element e)
  | Binding_variable(p,t1,t2)      -> "binding_" ^ (name_of_p p) ^ "_" ^ (name_of_t t1) ^ "_" ^ (name_of_t t2)
  | Local_repository_variable(l,r) -> "localrepository_" ^ (name_of_l l) ^ "_" ^ (name_of_r r)
  | Local_resource_variable(l,o)   -> "localresource_" ^ (name_of_l l) ^ "_" ^ (name_of_o o)
  | Location_used_variable(l)      -> "location_used_" ^ (name_of_l l)

  (* safe name creation *)
let name_var_id = ref 0
let name_of_variable_safe v = let id = !name_var_id in name_var_id := id + 1; "var_" ^ (string_of_int id)


(* 2. get all the variables in the constraint and optimization function *)

let get_named_variables vs =
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
      let (min, max) = (string_of_int (Data_helper.int_of_value (Bound.min bound)), string_of_int (Data_helper.int_of_value (Bound.max bound))) in
      res ^ ("var " ^ min  ^ ".." ^ max ^ " : "  ^ (v_map#get_name v) ^ ";\n")) 
    v_map#variables ("var 0.." ^ (string_of_int (Data_helper.int_of_value Value.infty)) ^ " : " ^ cost_variable_name ^ ";\n")

(* 4. core *)

let minizinc_of_unary_arith_op = function
  | Data_constraint.Abs -> "abs"

let minizinc_of_binary_arith_op = function
  | Data_constraint.Add -> "+"
  | Data_constraint.Sub -> "-"
  | Data_constraint.Mul -> "*"
  | Data_constraint.Div -> "/"
  | Data_constraint.Mod -> "%"

let minizinc_of_nary_arith_op = function
  | Data_constraint.Sum     -> "+"
  | Data_constraint.Product -> "*"

let minizinc_of_unit_of_nary_arith_op = function
  | Data_constraint.Sum     -> "0"
  | Data_constraint.Product -> "1"

let minizinc_of_arith_cmp_op = function
  | Data_constraint.Lt  -> "<"
  | Data_constraint.LEq -> "<="
  | Data_constraint.Eq  -> "="
  | Data_constraint.GEq -> ">="
  | Data_constraint.Gt  -> ">"
  | Data_constraint.NEq -> "!="

let minizinc_of_unary_konstraint_op = function
  | Data_constraint.Not -> "not"

let minizinc_of_binary_konstraint_op = function
  | Data_constraint.Implies -> "->"

let minizinc_of_nary_konstraint_op = function
  | Data_constraint.And -> "/\\"
  | Data_constraint.Or  -> "\\/"

let minizinc_of_unit_of_nary_konstraint_op = function
  | Data_constraint.And -> "true"
  | Data_constraint.Or  -> "false"

let rec minizinc_of_expression v_map e = match e with
  | Constant              (v)        -> (string_of_int (Data_helper.int_of_value v))
  | Variable              (v)        -> v_map#get_name v
  | Reified               (c)        -> Printf.sprintf "(bool2int %s)" (minizinc_of_konstraint v_map c)
  | UnaryArithExpression  (op,e)     -> Printf.sprintf "(%s(%s))" (minizinc_of_unary_arith_op op) (minizinc_of_expression v_map e)
  | BinaryArithExpression (op,e1,e2) -> Printf.sprintf "(%s %s %s)" (minizinc_of_expression v_map e1) (minizinc_of_binary_arith_op op) (minizinc_of_expression v_map e2)
  | NaryArithExpression   (op,l)     -> Printf.sprintf "(%s)"
                                                        (if l = [] 
                                                         then minizinc_of_unit_of_nary_arith_op op
                                                         else String.concat (" " ^ (minizinc_of_nary_arith_op op) ^ " ") (List.map (minizinc_of_expression v_map) l) )
and minizinc_of_konstraint v_map c = match c with
  | True                        -> "true"
  | False                       -> "false"
  | ArithKonstraint  (op,e1,e2) -> Printf.sprintf "(%s %s %s)" (minizinc_of_expression v_map e1) (minizinc_of_arith_cmp_op op) (minizinc_of_expression v_map e2)
  | UnaryKonstraint  (op,c)     -> Printf.sprintf "(%s (%s))" (minizinc_of_unary_konstraint_op op) (minizinc_of_konstraint v_map c)
  | BinaryKonstraint (op,c1,c2) -> Printf.sprintf "(%s %s %s)" (minizinc_of_konstraint v_map c1) (minizinc_of_binary_konstraint_op op) (minizinc_of_konstraint v_map c2)
  | NaryKonstraint   (op,l)     -> Printf.sprintf "(%s)"
                                                   (if l = []
                                                    then minizinc_of_unit_of_nary_konstraint_op op
                                                    else String.concat (" " ^ (minizinc_of_nary_konstraint_op op) ^ " ") (List.map (minizinc_of_konstraint v_map) l) )

let minizinc_of_konstraints v_map cs =
  List.fold_left (fun res (s,c) -> 
    (* TODO: This is a dirty hack, should just structure constraints better. *)
    let pre = ("\n%% " ^ s ^ "\n") in
    let minizinc_of_single_konstraint c = ("constraint " ^ (minizinc_of_konstraint v_map c) ^ ";") in
    let content = 
      match c with
      | Data_constraint.NaryKonstraint(Data_constraint.And, cs) -> String.concat "\n" (List.map minizinc_of_single_konstraint cs)
      | _                                                       -> minizinc_of_single_konstraint c 
    in
    pre ^ content ^ res
  ) "" cs

let output_of_variables v_map = 
  let output_of_variable v = "  \"" ^ v ^ " = \", show(" ^ v ^ "), \";\\n\"" in
  "output [\n" ^ (String.concat ",\n" (List.map output_of_variable (cost_variable_name::(Name_set.elements v_map#names)))) ^ "\n];\n\n"

let core_translation v_map f_bound cs =
  Zephyrus_log.log_solver_execution "Computing the variable declaration part of the file...\n";
  (* let decl = variable_declaration v_map f_bound in *)
  Zephyrus_log.log_solver_execution "Computing main part of the file...\n";
  let main = minizinc_of_konstraints v_map cs in
  Zephyrus_log.log_solver_execution "Computing the output part of the file...\n";
  let out = output_of_variables v_map in
 {
  mzn_variables        = v_map;
  mzn_declaration      = variable_declaration v_map f_bound; (* can take very very long with a lot of variables *)
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

let minizinc_goal_of_solve_goal v_map solve_goal = 
  match solve_goal with
  | Single_objective.Optimize(Maximize(e)) -> (Some(e), "solve maximize (" ^ cost_variable_name ^ ");")
  | Single_objective.Optimize(Minimize(e)) -> (Some(e), "solve minimize (" ^ cost_variable_name ^ ");")
  | Single_objective.Satisfy               -> (None, "solve satisfy;")

let add_optimization_goal smzn solve_goal = 
  let (eo,s) = minizinc_goal_of_solve_goal smzn.mzn_variables solve_goal in
    "%% ======= MiniZinc file automatically generated by Zephyrus =======\n\n"
  ^ "%% === Variables ===\n" ^ smzn.mzn_declaration ^ "\n\n"
  ^ "%% === Main Constraints ===\n" ^ smzn.mzn_main_constraint ^ "\n\n"
  ^ "%% === Extra Constraints ===" ^ smzn.mzn_extra_constraint ^ "\n" ^ ("") ^ "\n\n"
  ^ "%% === Optimization function ===\n"
    ^ (match eo with None -> "" | Some(e) ->"constraint " ^ cost_variable_name ^ " = " ^ (minizinc_of_expression smzn.mzn_variables e) ^ ";\n\n") ^ s ^ "\n\n\n"
  ^ "%% === Output definition ===\n" ^ smzn.mzn_output ^ "\n" 




