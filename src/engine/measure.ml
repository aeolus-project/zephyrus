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

(** Measure the model. *)

(* ORDER IS IMPORTANT (we need the "optimization_function" type from the Data_model not Data_constraint) *)
open Data_constraint
open Data_model

(*
module Value = struct
  type t = value
  
  let zero     = Finite_value 0
  let one      = Finite_value 1
  let infty    = Infinite_value
  let of_int n = Finite_value n

  let min v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (min n1 n2)
    | (Finite_value _ , Infinite_value ) -> v1
    | (Infinite_value , Finite_value _ ) -> v2
    | (Infinite_value , Infinite_value ) -> infty

  let mins l = Data_common.List.fold_combine (fun x -> x) min l infty

  let max v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (max n1 n2)
    | _ -> Infinite_value

  let maxs l = Data_common.List.fold_combine (fun x -> x) max l zero

  let add v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 + n2)
    | _ -> Infinite_value
  let sum l = Data_common.List.fold_combine (fun x -> x) add l zero

  let multiply v1 v2 = match (v1,v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (n1 * n2)
    | _ -> Infinite_value
  let product l = Data_common.List.fold_combine (fun x -> x) multiply l one
  
  let sub v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> of_int (Pervasives.max 0 (n1 - n2)) (* we want to stay positive :p *)
    | (Finite_value _ , _              ) -> zero
    | (_              , Finite_value _ ) -> infty
    | _  -> zero

  let div v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> of_int ((n1 / n2)
    | (Finite_value _ , _              ) -> zero
    | (_              , Finite_value _ ) -> infty
    | _  -> of_int 1
  
  let is_finite v = (v != Infinite_value)
  let is_infty  v = (v  = Infinite_value)
  
  let is_sup v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> v1 > v2
    | (Finite_value _ , _              ) -> false
    | (_              , Finite_value _ ) -> true
    | _  -> false
    
  let is_inf_eq v1 v2 = match (v1, v2) with
    | (Finite_value n1, Finite_value n2) -> v1 <= v2
    | (Finite_value _ , _              ) -> true
    | (_              , Finite_value _ ) -> false
    | _  -> true
  
  let int_of v default = match v with | Finite_value v -> v | Infinite_value -> default
  let int_of_unsafe v  = match v with | Finite_value v -> v | Infinite_value -> raise (Failure "cannot give a normal value for an infinite one")
  
  let abs v = match v with
    | Finite_value v' -> if v' >= 0 then Finite_value v' else Finite_value (-v')
    | Infinite_value -> Infinite_value

end
*)

class model ?(with_packages = true) ~universe ~configuration () = object (self : 'selftype)

  val with_packages : bool          = with_packages

  val universe      : universe      = universe
  val configuration : configuration = configuration
 
  (** [number_of_packages_on_location location_id package_id] returns [1] if there is a package with id [package_id] installed on the location with id [location_id] and [0] otherwise. *)
  method number_of_packages_on_location location_id package_id : int =
    if Package_id_set.mem package_id (configuration#get_location location_id)#packages_installed then 1 else 0

  (** [number_of_packages_global package_id] returns the total number of packages with id [package_id] installed on all the locations. *)
  method number_of_packages_global package_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (self#number_of_packages_on_location location_id package_id)
    ) configuration#get_location_ids 0

  (** [number_of_any_packages_global ()] returns the total number of packages installed on all the locations. *)
  method number_of_any_packages_global () : int = 
    Location_id_set.fold (fun location_id sum ->
      sum + (Package_id_set.cardinal (self#get_location_package_ids location_id))
    ) configuration#get_location_ids 0

  (** [number_of_components_on_location location_id component_type_id] returns the number of components of type with id [component_type_id] installed on the location with id [location_id]. *)
  method number_of_components_on_location location_id component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      (component#location = location_id) && (component#typ = component_type_id)
    ) configuration#get_component_ids)

  (** [number_of_components_global component_type_id] returns the total number of components of type with id [component_type_id] installed on all the locations. *)
  method number_of_components_global component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      component#typ = component_type_id
    ) configuration#get_component_ids)
  
  (** [number_of_any_components_global ()] returns the total number of components installed on all the locations. *)
  method number_of_any_components_global () : int =
    Component_id_set.cardinal configuration#get_component_ids

  (** [number_of_ports_provided_on_location location_id port_id] returns the total number of port arity of port [port_id] provided by all the components installed on the location with id [location_id]. *)
  method number_of_ports_provided_on_location location_id port_id : int =
    let int_of_provide_arity = function
      | Infinite_provide -> max_int
      | Finite_provide a' -> a'
    in
    Component_id_set.fold (fun component_id sum -> 
      let component = configuration#get_component component_id in
      let provide_arity =
        if component#location = location_id then 
          let component_type = universe#get_component_type (component#typ) in
          if Port_id_set.mem port_id component_type#provide_domain
          then int_of_provide_arity (component_type#provide port_id)
          else 0
        else 0 in
      (sum + provide_arity)
    ) configuration#get_component_ids 0

  (** [number_of_ports_provided_global port_id] returns the total number of port arity of port [port_id] provided by all the components installed on all the locations. *)
  method number_of_ports_provided_global port_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (self#number_of_ports_provided_on_location location_id port_id)
    ) configuration#get_location_ids 0

  (** [get_location_component_ids location_id] returns a set of component ids of all the components installed on the location with id [location_id]. *)
  method get_location_component_ids location_id : Component_id_set.t =
    Component_id_set.filter (fun component_id ->
      let component = configuration#get_component component_id in
      component#location = location_id
    ) configuration#get_component_ids

  (** [get_location_package_ids location_id] returns a set of package ids of all the packages installed on the location with id [location_id]. *)
  method get_location_package_ids location_id : Package_id_set.t =
    let location = configuration#get_location location_id in
    location#packages_installed

  (** [location_is_empty location_id] returns [true] if there are no components (nor packages if the value [with_packages] is [true]) installed on the location with id [location_id], [false] otherwise. *)  
  method location_is_empty location_id : bool =
    (Component_id_set.is_empty (self#get_location_component_ids location_id))
    &&
    (if with_packages
     then (Package_id_set.is_empty (self#get_location_package_ids location_id))
     else true)

  (** [location_is_used location_id] returns true if there are any components (or packages if the value [with_packages] is [true]) installed on the location with id [location_id], [false] otherwise. *)  
  method location_is_used location_id : bool =
    not (self#location_is_empty location_id)

  (** [get_used_location_ids ()] returns a set of location ids of all the locations which are used, i.e. have any components (or packages if the value [with_packages] is [true]) installed on them. *)
  method get_used_location_ids () : Location_id_set.t =
    Location_id_set.filter (self#location_is_used) configuration#get_location_ids

  (** [used_locations_cost ()] returns the sum of costs of all the used locations, i.e. those which have any components (or packages if the value [with_packages] is [true]) installed on them. *)
  method used_locations_cost () : location_cost =
    Location_id_set.fold (fun location_id sum ->
      let location = configuration#get_location location_id in
      sum + location#cost
    ) (self#get_used_location_ids ()) 0

  (** [number_of_bindings port_id requiring_component_type_id providing_component_type_id] returns the number of bindings on port [port_id] between the requiring components of type [requiring_component_type_id] and providing components of type [providing_component_type_id]. *)
  method number_of_bindings port_id requiring_component_type_id providing_component_type_id : int =
    Binding_set.cardinal (
      Binding_set.filter (fun binding ->
        binding#port = port_id &&
        ((configuration#get_component binding#requirer)#typ = requiring_component_type_id) &&
        ((configuration#get_component binding#provider)#typ = providing_component_type_id)
      ) (configuration#get_bindings))

  (** [value_of_variable variable] returns the value bound to the variable [variable] in our configuration. *)
  method value_of_variable (variable : Data_constraint.variable) : Data_constraint.value =
    let reify_bool b = if b then Data_constraint.Finite_value 1 else Data_constraint.Finite_value 0 in
    match variable with
    | Simple_variable spec_variable_name -> assert(false) (* TODO *)

    | Global_variable (Component_type component_type_id) -> Data_constraint.Finite_value (self#number_of_components_global     component_type_id)
    | Global_variable (Port           port_id          ) -> Data_constraint.Finite_value (self#number_of_ports_provided_global port_id)
    | Global_variable (Package        package_id       ) -> Data_constraint.Finite_value (self#number_of_packages_global       package_id)

    | Local_variable (location_id, (Component_type component_type_id)) -> Data_constraint.Finite_value (self#number_of_components_on_location     location_id component_type_id)
    | Local_variable (location_id, (Port           port_id          )) -> Data_constraint.Finite_value (self#number_of_ports_provided_on_location location_id port_id)
    | Local_variable (location_id, (Package        package_id       )) -> Data_constraint.Finite_value (self#number_of_packages_on_location       location_id package_id)

    | Binding_variable (port_id, requiring_component_type_id, providing_component_type_id) -> 
        Data_constraint.Finite_value (self#number_of_bindings port_id requiring_component_type_id providing_component_type_id)

    | Local_repository_variable (location_id, repository_id) -> 
        reify_bool ((configuration#get_location location_id)#repository = repository_id)

    | Local_resource_variable (location_id, resource_id) -> 
        Data_constraint.Finite_value ((configuration#get_location location_id)#provide_resources resource_id)

    | Location_used_variable (location_id) -> 
        reify_bool (self#location_is_used location_id)

  (** [value_of_expression expression] returns the value to which the given expression [expression] evaluates in our configuration. *)
  method value_of_expression (expression : Data_constraint.expression) : Data_constraint.value =
    let reify_bool b = if b then Data_constraint.Finite_value 1 else Data_constraint.Finite_value 0 in
    match expression with
    | Constant              value -> value
    | Variable              variable -> self#value_of_variable variable
    | Reified               k -> reify_bool (self#bool_of_konstraint k)
    
    | UnaryArithExpression  (unary_arith_op, e) -> 
       (match unary_arith_op with
        | Abs -> Value.abs (self#value_of_expression e))

    | BinaryArithExpression (binary_arith_op, e1, e2) -> 
      let perform_op : value -> value -> value = 
        (match binary_arith_op with
        | Add -> Value.sum
        | Sub -> Value.sub
        | Mul -> Value.prod
        | Div -> Value.div  (* Attention: this implementation of div rounds up! *)
        | Mod -> Value.modulo) in
      perform_op (self#value_of_expression e1) (self#value_of_expression e2)

    | NaryArithExpression   (nary_arith_op, es) ->
      let perform_op : value list -> value = 
        (match nary_arith_op with
        | Sum     -> Value.sums
        | Product -> Value.prods) in
      perform_op (List.map self#value_of_expression es)

  (** [bool_of_konstraint konstraint] returns if the given constraint [konstraint] in true or false in our configuration. *)
  method bool_of_konstraint (konstraint : Data_constraint.konstraint) : bool = 
    match konstraint with
    | True                  -> true
    | False                 -> false
    
    | ArithKonstraint  (arith_cmp_op, e1, e2) -> 
      let perform_op : value -> value -> bool = 
        (match arith_cmp_op with
        | Lt  -> Value.lt
        | LEq -> Value.leq
        | Eq  -> Value.eq
        | GEq -> Value.geq
        | Gt  -> Value.gt
        | NEq -> Value.neq) in
      perform_op (self#value_of_expression e1) (self#value_of_expression e2)

    | UnaryKonstraint  (unary_konstraint_op, k) ->
      let perform_op : bool -> bool = 
        (match unary_konstraint_op with
        | Not -> not) in
      perform_op (self#bool_of_konstraint k)

    | BinaryKonstraint (binary_konstraint_op, k1, k2) -> 
      let perform_op : bool -> bool -> bool = 
        (match binary_konstraint_op with
        | Implies -> (fun b1 b2 -> (not b1) || b2) ) in
      perform_op (self#bool_of_konstraint k1) (self#bool_of_konstraint k2)

    | NaryKonstraint (nary_konstraint_op, ks) -> 
      let perform_op : bool list -> bool = 
        (match nary_konstraint_op with
        | And -> List.for_all (fun b -> b)
        | Or  -> List.exists  (fun b -> b)) in
      perform_op (List.map self#bool_of_konstraint ks)

  method evaluate_single_objective_optimization (single_objective_optimization : expression Single_objective.optimization) : value Single_objective.optimization =
    match single_objective_optimization with
    | Single_objective.Minimize (e) -> Single_objective.Minimize (self#value_of_expression e)
    | Single_objective.Maximize (e) -> Single_objective.Maximize (self#value_of_expression e)

  method evaluate_multi_objective_optimization (multi_objective_optimization : expression Multi_objective.optimization) : value Multi_objective.optimization =
    match multi_objective_optimization with
    | Multi_objective.Single        (single_objective_optimization)                               -> Multi_objective.Single        (self#evaluate_single_objective_optimization single_objective_optimization)
    | Multi_objective.Lexicographic (single_objective_optimization, multi_objective_optimization) -> Multi_objective.Lexicographic (self#evaluate_single_objective_optimization single_objective_optimization, self#evaluate_multi_objective_optimization multi_objective_optimization)

  method evaluate_multi_objective_solve_goal (multi_objective_solve_goal : expression Multi_objective.solve_goal) : value Multi_objective.solve_goal =
    match multi_objective_solve_goal with
    | Multi_objective.Satisfy                                 -> Multi_objective.Satisfy
    | Multi_objective.Optimize (multi_objective_optimization) -> Multi_objective.Optimize (self#evaluate_multi_objective_optimization multi_objective_optimization)

end


type measured_optimization_function = value Multi_objective.solve_goal

(*
(* TODO: Probably remove. *)

type optimality_measure = int list

let measure_optimality ?(with_packages = true) (universe : universe) (configuration : configuration) (optimization_function : optimization_function) : optimality_measure =
  let measure_model = new model ~with_packages ~universe ~configuration () in
  match optimization_function with

  | Optimization_function_simple -> 
      [measure_model#number_of_any_components_global ()] (* Minimize the number of components. *)
  
  | Optimization_function_compact -> 
      [measure_model#used_locations_cost ();             (* First minimize the number of used locations, *) 
       measure_model#number_of_any_components_global (); (* then minimize the number of components, *)
       measure_model#number_of_any_packages_global ()]   (* finally minimize the number of packages (so we do not have useless packages). *)

  | Optimization_function_conservative -> [] (* TODO *)
  | Optimization_function_spread       -> [] (* TODO *)
  | Optimization_function_none         -> [] (* TODO *)
*)

let measured_optimization_function_of_constraint_optimization_function ?(with_packages = true) (universe : universe) (configuration : configuration) (constraint_optimization_function : expression Multi_objective.solve_goal) : measured_optimization_function =
  let measure_model = new model ~with_packages ~universe ~configuration () in
  measure_model#evaluate_multi_objective_solve_goal constraint_optimization_function