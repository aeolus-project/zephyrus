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

module type S = sig
  val string_list : string list -> string
  val int_list    : int    list -> string

  (*/********************************\*)
  (*  Model                           *)
  (*\********************************/*)

  (** 1. Resources *)
  val resource_name     : Data_model.resource_name       -> string
  val resource_name_set : Data_model.Resource_name_set.t -> string
  val resource_id       : Data_model.resource_id         -> string
  val resource_id_set   : Data_model.Resource_id_set.t   -> string

  val resource_provide_arity : Data_model.resource_provide_arity -> string
  val resource_consume_arity : Data_model.resource_consume_arity -> string

  (** 2. Component types *)
  val port_name     : Data_model.port_name       -> string
  val port_name_set : Data_model.Port_name_set.t -> string
  val port_id       : Data_model.port_id         -> string
  val port_id_set   : Data_model.Port_id_set.t   -> string
  val port_id_map   : ('a -> string) -> 'a Data_model.Port_id_map.t -> string
  val port          : Data_model.port            -> string

  val component_type_name     : Data_model.component_type_name       -> string
  val component_type_name_set : Data_model.Component_type_name_set.t -> string
  val component_type_id       : Data_model.component_type_id         -> string
  val component_type_id_set   : Data_model.Component_type_id_set.t   -> string
  val component_type_id_map   : ('a -> string) -> 'a Data_model.Component_type_id_map.t -> string

  val provide_arity : Data_model.provide_arity -> string
  val require_arity : Data_model.require_arity -> string

  (** 3. Packages *)
  val package_name     : Data_model.package_name       -> string
  val package_name_set : Data_model.Package_name_set.t -> string
  val package_id       : Data_model.package_id         -> string
  val package_id_set   : Data_model.Package_id_set.t   -> string

  (** 4. Repositories *)
  val repository_name     : Data_model.repository_name       -> string
  val repository_name_set : Data_model.Repository_name_set.t -> string
  val repository_id       : Data_model.repository_id         -> string
  val repository_id_set   : Data_model.Repository_id_set.t   -> string

  (** 5. Location *)
  val location_name     : Data_model.location_name       -> string
  val location_name_set : Data_model.Location_name_set.t -> string
  val location_id       : Data_model.location_id         -> string
  val location_id_set   : Data_model.Location_id_set.t   -> string

  val location_categories : Location_categories.t -> string

  val component_name     : Data_model.component_name       -> string
  val component_name_set : Data_model.Component_name_set.t -> string
  val component_id       : Data_model.component_id         -> string
  val component_id_set   : Data_model.Component_id_set.t   -> string


  (************************************)
  (** Specification                   *)
  (************************************)

  val spec_variable_name : Data_model.spec_variable_name -> string
  val spec_const         : Data_model.spec_const         -> string

  val specification : Data_model.specification -> string

  val model_optimization_function : Data_model.optimization_function -> string

  (************************************)
  (** Constraints                     *)
  (************************************)


  val element       : Data_constraint.element -> string
  val local_element : Data_model.location_id -> Data_constraint.element -> string

  val variable   : Data_constraint.variable   -> string
  val value      : Data_constraint.value      -> string
  val expression : Data_constraint.expression -> string
  val konstraint : Data_constraint.konstraint -> string

  val structured_constraints : Data_state.structured_constraints -> string

  val constraint_single_optimization   : Data_constraint.Single_objective.optimization -> string
  val constraint_single_solve_goal     : Data_constraint.Single_objective.solve_goal   -> string
  val constraint_multi_optimization    : Data_constraint.Multi_objective.optimization  -> string
  val constraint_multi_solve_goal      : Data_constraint.Multi_objective.solve_goal    -> string
  val constraint_optimization_function : Data_constraint.optimization_function         -> string

  val bound : Data_constraint.Bound.t -> string

  (************************************)
  (** Solution                        *)
  (************************************)

  val solution : Data_constraint.solution -> string


  (************************************)
  (** Configuration                   *)
  (************************************)

  val configuration : Data_model.universe -> Data_model.configuration -> string

  val model_catalog : Data_model_catalog.closed_model_catalog -> string
end


(** Identifier printing *)

module type STRING_OF_ID = sig
  val resource_id       : Data_model.resource_id       -> string
  val port_id           : Data_model.port_id           -> string
  val component_type_id : Data_model.component_type_id -> string
  val package_id        : Data_model.package_id        -> string
  val repository_id     : Data_model.repository_id     -> string
  val location_id       : Data_model.location_id       -> string
  val component_id      : Data_model.component_id      -> string
end

module Quoted_string_of_id =
  functor (String_of_id : STRING_OF_ID) -> struct

  let quote string_of_id id = Printf.sprintf "\"%s\"" (string_of_id id)

  let resource_id       = quote String_of_id.resource_id
  let port_id           = quote String_of_id.port_id
  let component_type_id = quote String_of_id.component_type_id
  let package_id        = quote String_of_id.package_id
  let repository_id     = quote String_of_id.repository_id
  let location_id       = quote String_of_id.location_id
  let component_id      = quote String_of_id.component_id
end


(** The String_of modules can be parametrized by Sotring_of_id modules. *)

module Make =
  functor (String_of_id : STRING_OF_ID) -> struct

  open Data_model

  let identity = fun x -> x

  let string_list       s = "{" ^ (String.concat ", " s) ^ "}"
  let int_list          s = string_list (List.map string_of_int s)
  let string_assoc_list s = string_list (List.map (fun (k,v) -> Printf.sprintf "%s: %s" k v) s)

  (************************************)
  (** Model                           *)
  (************************************)

  (** 1. Resources *)
  let resource_name         = identity
  let resource_name_set set = string_list (List.map resource_name (Resource_name_set.elements set))
  let resource_id           = String_of_id.resource_id
  let resource_id_set   set = string_list (List.map resource_id (Resource_id_set.elements set))

  let resource_provide_arity r = string_of_int r
  let resource_consume_arity r = string_of_int r

  (** 2. Component types *)
  let port_name         = identity
  let port_name_set set = string_list (List.map port_name (Port_name_set.elements set))
  let port_id           = String_of_id.port_id
  let port_id_set   set = string_list (List.map port_id (Port_id_set.elements set))
  let port_id_map string_of_value map = string_assoc_list (List.map (fun (k,v) -> (port_id k, string_of_value v) ) (Port_id_map.bindings map))

  let port          = port_id

  let component_type_name         = identity
  let component_type_name_set set = string_list (List.map component_type_name (Component_type_name_set.elements set))
  let component_type_id           = String_of_id.component_type_id
  let component_type_id_set   set = string_list (List.map component_type_id (Component_type_id_set.elements set))
  let component_type_id_map string_of_value map = string_assoc_list (List.map (fun (k,v) -> (component_type_id k, string_of_value v) ) (Component_type_id_map.bindings map))

  let provide_arity arity = match arity with | Data_model.Infinite_provide -> "infinite" | Data_model.Finite_provide i -> string_of_int i
  let require_arity       = string_of_int

  (** 3. Packages *)
  let package_name         = identity
  let package_name_set set = string_list (List.map package_name (Package_name_set.elements set))
  let package_id           = String_of_id.package_id
  let package_id_set   set = string_list (List.map package_id (Package_id_set.elements set))

  (** 4. Repositories *)
  let repository_name         = identity
  let repository_name_set set = string_list (List.map repository_name (Repository_name_set.elements set))
  let repository_id           = String_of_id.repository_id
  let repository_id_set   set = string_list (List.map repository_id (Repository_id_set.elements set))

  (** 5. Location *)
  let location_name         = identity
  let location_name_set set = string_list (List.map location_name (Location_name_set.elements set))
  let location_id           = String_of_id.location_id
  let location_id_set   set = string_list (List.map location_id (Location_id_set.elements set))

  let location_category   set = string_list (List.map location_id (Location_id_set.elements set))
  let location_categories set = string_list (List.map location_category (Location_categories.elements set))

  let component_name         = identity
  let component_name_set set = string_list (List.map component_name (Component_name_set.elements set))
  let component_id           = String_of_id.component_id
  let component_id_set   set = string_list (List.map component_id (Component_id_set.elements set))

  (** Specification *)
  let spec_variable_name v     = v
  let spec_const         const = string_of_int const

  let spec_local_element e = match e with
    | Data_model.Spec_local_element_package        (k) -> "#" ^ (package_id k)
    | Data_model.Spec_local_element_component_type (c) -> "#" ^ (component_type_id c)
    | Data_model.Spec_local_element_port           (p) -> "#" ^ (port_id p)

  let rec spec_local_expr e = match e with
    | Data_model.Spec_local_expr_var   (v)      -> spec_variable_name v
    | Data_model.Spec_local_expr_const (c)      -> spec_const c
    | Data_model.Spec_local_expr_arity (e)      -> spec_local_element e
    | Data_model.Spec_local_expr_add   (e1, e2) -> "(" ^ (spec_local_expr e1) ^ " + " ^ (spec_local_expr e2) ^ ")"
    | Data_model.Spec_local_expr_sub   (e1, e2) -> "(" ^ (spec_local_expr e1) ^ " - " ^ (spec_local_expr e2) ^ ")"
    | Data_model.Spec_local_expr_mul   (e1, e2) -> (spec_const e1) ^ " * " ^ (spec_local_expr e2)

  let spec_op o = match o with
    | Data_model.Lt  -> " < "  
    | Data_model.LEq -> " <= " 
    | Data_model.Eq  -> " = "
    | Data_model.GEq -> " >= " 
    | Data_model.Gt  -> " > "  
    | Data_model.NEq -> " <> "

  let rec local_specification s = match s with
    | Data_model.Spec_local_true              -> "true"
    | Data_model.Spec_local_op   (e1, op, e2) -> "(" ^ (spec_local_expr e1) ^ (spec_op op) ^ (spec_local_expr e2) ^ ")"
    | Data_model.Spec_local_and  (s1, s2)     -> "(" ^ (local_specification s1) ^ " /\\ " ^ (local_specification s2) ^ ")"
    | Data_model.Spec_local_or   (s1, s2)     -> "(" ^ (local_specification s1) ^ " \\/ " ^ (local_specification s2) ^ ")"
    | Data_model.Spec_local_impl (s1, s2)     -> "(" ^ (local_specification s1) ^ " => " ^ (local_specification s2) ^ ")"
    | Data_model.Spec_local_not  (s')         -> "not " ^ (local_specification s')

  let spec_resource_constraint   co = String.concat "; " (List.map (fun (o, op, i) -> (resource_id o) ^ (spec_op op) ^ (resource_provide_arity i)) co)
  let spec_repository_constraint cr = match cr with
    | [] -> "_" 
    | _ -> String.concat " \\/ " (List.map (fun r -> (repository_id r)) cr)

  let spec_element e = match e with
    | Data_model.Spec_element_package        (k)          -> "#" ^ (package_id k)
    | Data_model.Spec_element_component_type (c)          -> "#" ^ (component_type_id c)
    | Data_model.Spec_element_port           (p)          -> "#" ^ (port_id p)
    | Data_model.Spec_element_location       (co, cr, ls) -> "#(" ^ (spec_resource_constraint co) ^ "){" ^ (spec_repository_constraint cr) ^ " : " ^ (local_specification ls) ^ "}"

  let rec spec_expr e = match e with
    | Data_model.Spec_expr_var   (v)      -> spec_variable_name v
    | Data_model.Spec_expr_const (c)      -> spec_const c
    | Data_model.Spec_expr_arity (e)      -> spec_element e
    | Data_model.Spec_expr_add   (e1, e2) -> "(" ^ (spec_expr e1) ^ " + " ^ (spec_expr e2) ^ ")"
    | Data_model.Spec_expr_sub   (e1, e2) -> "(" ^ (spec_expr e1) ^ " - " ^ (spec_expr e2) ^ ")"
    | Data_model.Spec_expr_mul   (e1, e2) -> (spec_const e1) ^ " * " ^ (spec_expr e2)

  let rec specification s = match s with
    | Data_model.Spec_true              -> "true"
    | Data_model.Spec_op   (e1, op, e2) ->  (spec_expr e1) ^ (spec_op op) ^ (spec_expr  e2)
    | Data_model.Spec_and  (s1, s2)     -> "(" ^ (specification s1) ^ " /\\ " ^ (specification s2) ^ ")"
    | Data_model.Spec_or   (s1, s2)     -> "(" ^ (specification s1) ^ " \\/ " ^ (specification s2) ^ ")"
    | Data_model.Spec_impl (s1, s2)     -> "(" ^ (specification s1) ^ " => "  ^ (specification s2) ^ ")"
    | Data_model.Spec_not  (s')         -> "not " ^ (specification s')
    | Spec_everywhere              (ls) -> "everywhere(" ^ (local_specification ls) ^ ")"
    | Spec_at        (location_ids, ls) ->         "at(" ^ (local_specification ls) ^ ")"


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
    | Data_constraint.Component_type (t) -> "N(" ^ (component_type_id t) ^ ")"
    | Data_constraint.Port           (p) -> "N(" ^ (port_id p) ^ ")"
    | Data_constraint.Package        (k) -> "N(" ^ (package_id k) ^ ")"

  let local_element l e = match e with
    | Data_constraint.Component_type (t) -> "N(" ^ (location_id l) ^ ", " ^ (component_type_id t) ^ ")"
    | Data_constraint.Port           (p) -> "N(" ^ (location_id l) ^ ", " ^ (port_id p) ^ ")"
    | Data_constraint.Package        (k) -> "N(" ^ (location_id l) ^ ", " ^ (package_id k) ^ ")"

  let variable v = match v with 
    | Data_constraint.Simple_variable           (v)       -> spec_variable_name v
    | Data_constraint.Global_variable           (e)       -> element e
    | Data_constraint.Local_variable            (l,e)     -> local_element l e
    | Data_constraint.Binding_variable          (p,t1,t2) -> "B(" ^ (port_id p) ^ ", " ^ (component_type_id t1) ^ ", " ^ (component_type_id t2) ^ ")"
    | Data_constraint.Local_repository_variable (l,r)     -> "R(" ^ (location_id l) ^ ", " ^ (repository_id r) ^ ")"
    | Data_constraint.Local_resource_variable   (l,r)     -> "O(" ^ (location_id l) ^ ", " ^ (resource_id r) ^ ")"
    | Data_constraint.Location_used_variable    (l)       -> "U(" ^ (location_id l) ^ ")"

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

  let structured_constraints structured_constraints =
    let lines =
      List.flatten (
        List.map (fun (description_string, constraint_list) -> 
          let constraint_lines = List.map konstraint constraint_list in
          description_string :: constraint_lines
        ) structured_constraints ) in
    String.concat "" (List.map (fun line -> Printf.sprintf "%s\n" line) lines)

  let constraint_single_optimization (single_optimization : Data_constraint.Single_objective.optimization) =
    match single_optimization with 
    | Data_constraint.Single_objective.Minimize e -> Printf.sprintf "Minimize %s" (expression e)
    | Data_constraint.Single_objective.Maximize e -> Printf.sprintf "Maximize %s" (expression e)
  
  let constraint_single_solve_goal (single_solve_goal : Data_constraint.Single_objective.solve_goal) =    
    match single_solve_goal with
    | Data_constraint.Single_objective.Satisfy               -> "Satisfy"
    | Data_constraint.Single_objective.Optimize optimization -> Printf.sprintf "Optimize (%s)" (constraint_single_optimization optimization)

  let rec constraint_multi_optimization (multi_optimization : Data_constraint.Multi_objective.optimization) =
    match multi_optimization with 
    | Data_constraint.Multi_objective.Single         single_optimization                ->                          constraint_single_optimization single_optimization
    | Data_constraint.Multi_objective.Lexicographic (single_optimization, optimization) -> Printf.sprintf "%s; %s" (constraint_single_optimization single_optimization) (constraint_multi_optimization optimization)

  let constraint_multi_solve_goal (multi_solve_goal : Data_constraint.Multi_objective.solve_goal) =
    match multi_solve_goal with
    | Data_constraint.Multi_objective.Satisfy               -> "Satisfy"
    | Data_constraint.Multi_objective.Optimize optimization -> Printf.sprintf "Optimize (%s)" (constraint_multi_optimization optimization)

  let constraint_optimization_function = constraint_multi_solve_goal


  let bound b = "(" ^ (value (Data_constraint.Bound.min b))  ^ ", " ^ (value (Data_constraint.Bound.max b)) ^ ")"

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
    
    Printf.printf "\n%s\n" (repository_id_set universe#get_repository_ids);

    let location_ids = Location_id_set.elements configuration#get_location_ids in
    
    let location_strings = List.map (fun lid ->

      let l = configuration#get_location lid in
      (* assert(configuration#get_location_id l#name = lid); *)
      (* assert(configuration#get_location_name lid = l#name); *)
      let line1 = Printf.sprintf "%s -> %s\n" (location_id lid) (location_name (Name_of.location_id lid)) in

      let rid = l#repository in
      let r = universe#get_repository rid in
      (* assert(universe#get_repository_id r#name = rid); *)
      (* assert(universe#get_repository_name rid = r#name); *)
      let line2 = Printf.sprintf "    + repository : %s -> %s\n" (repository_id rid) (repository_name (Name_of.repository_id rid)) in

      let kids = l#packages_installed in
      let module Package_name_set_of_package_id_set = Data_common.Set.Convert(Package_id_set)(Package_name_set) in
      let knames = Package_name_set_of_package_id_set.convert (fun package_id -> Name_of.package_id package_id) kids in
      let line3 = Printf.sprintf "    + packages : %s\n" (package_name_set knames) in

      Printf.sprintf "%s%s%s" line1 line2 line3
    ) location_ids in

    String.concat "" location_strings


    let model_catalog model_catalog = (* TODO: Not safe, as when we remove names, String_of will use the catalog itself for printing ... *)

      let module Component_type_id_map_extract_key   = Data_model.Component_type_id_map  .Set_of_keys(Data_model.Component_type_id_set)     in
      let module Component_type_name_map_extract_key = Data_model.Component_type_name_map.Set_of_keys(Data_model.Component_type_name_set)   in
      let module Extract_package_names               = Data_common.Set.Convert(Data_model_catalog.Repository_id_package_name_set)(Data_model.Package_name_set) in
      
      String.concat "\n" [
        "component_types";
        string_list (List.map component_type_id   (Data_model.Component_type_id_set  .elements model_catalog#component_type#ids));
        string_list (List.map component_type_name (Data_model.Component_type_name_set.elements model_catalog#component_type#names));

        "ports";
        string_list (List.map port_id   (Data_model.Port_id_set  .elements model_catalog#port#ids));
        string_list (List.map port_name (Data_model.Port_name_set.elements model_catalog#port#names));

        "repositories";
        string_list (List.map repository_id   (Data_model.Repository_id_set  .elements model_catalog#repository#ids));
        string_list (List.map repository_name (Data_model.Repository_name_set.elements model_catalog#repository#names));

        "packages";
        string_list (List.map package_id   (Data_model.Package_id_set  .elements model_catalog#package#ids));
        string_list (List.map package_name (Data_model.Package_name_set.elements (Extract_package_names.convert snd model_catalog#package#names)));

        "resources";
        string_list (List.map resource_id   (Data_model.Resource_id_set  .elements model_catalog#resource#ids));
        string_list (List.map resource_name (Data_model.Resource_name_set.elements model_catalog#resource#names));

        "locations";
        string_list (List.map location_id   (Data_model.Location_id_set  .elements model_catalog#location#ids));
        string_list (List.map location_name (Data_model.Location_name_set.elements model_catalog#location#names));

        "components";
        string_list (List.map component_id   (Data_model.Component_id_set  .elements model_catalog#component#ids));
        string_list (List.map component_name (Data_model.Component_name_set.elements model_catalog#component#names));
      ]
end


(** Print identifiers directly as numbers. *)
module Id_string_of_id : STRING_OF_ID = struct
  let resource_id       = string_of_int
  let port_id           = string_of_int
  let component_type_id = string_of_int
  let package_id        = string_of_int
  let repository_id     = string_of_int
  let location_id       = string_of_int
  let component_id      = string_of_int
end

module Id_string_of        = Make(Id_string_of_id)                      (** Print identifiers directly as numbers. *)
module Quoted_id_string_of = Make(Quoted_string_of_id(Id_string_of_id)) (** Print identifiers as quoted numbers. *)


(** Translate identifiers to names. *)
module Name_string_of_id : STRING_OF_ID = Name_of

module Name_string_of        = Make(Name_string_of_id)                      (** Translate identifiers to names. *)
module Quoted_name_string_of = Make(Quoted_string_of_id(Name_string_of_id)) (** Translate identifiers to names and add quotes around. *)

include Id_string_of