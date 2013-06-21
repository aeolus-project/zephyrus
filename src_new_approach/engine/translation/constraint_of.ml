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
    - datatypes/Data_constraint
    - datatypes/Data_model
    - datatypes/Functions_model
*)

open Data_constraint

(*******************************************)
(** 1. Helper functions                    *)
(*******************************************)

let value_of_provide_arity a = match a with Data_model.Infinite_provide -> Infinite_value | Data_model.Finite_provide(i) -> Finite_value(i)
let value_of_require_arity a = Finite_value(a)
let value i = Finite_value(i)
let infinite_value = Infinite_value

let constant i = Constant(Finite_value(i))
let constant_of_provide_arity a = Constant(value_of_provide_arity a)
let constant_of_require_arity a = Constant(value_of_require_arity a)

let var2expr   v = Variable v
let const2expr c = Constant c
let int2expr   i = Constant (Finite_value i)

let ( +~ )    x y  = Add([x; y])
let ( -~ )    x y  = Sub(x, y)
let ( *~ )    x y  = Mul([x; y])
let ( /~ )    x y  = Div(x, y)
let ( %~ )    x y  = Mod(x, y)
let abs       x    = Abs(x)
let sum       l    = Add(l)
let reify     cstr = Reified(cstr)

let (  <~ )   x y  = Arith (x, Lt,  y)
let ( <=~ )   x y  = Arith (x, LEq, y)
let (  =~ )   x y  = Arith (x, Eq,  y)
let ( >=~ )   x y  = Arith (x, GEq, y)
let (  >~ )   x y  = Arith (x, Gt,  y)
let ( <>~ )   x y  = Arith (x, NEq, y)

let truecstr   = True

let (  &&~~ ) x y  = And([x; y])
let (  ||~~ ) x y  = Or([x; y])
let (  =>~~ ) x y  = Implies(x, y)
let not       x    = Not(x)



let get_provide_arity t p = constant_of_provide_arity (t#provide p)
let get_require_arity t p = constant_of_require_arity (t#require p)
let get_consume x o = constant (x#consume o)

(* naming convention from the paper *)
let eNt t = Variable(Global_variable(Component_type(t)))
let eNp p = Variable(Global_variable(Port(p)))
let eNk k = Variable(Global_variable(Package(k)))
let eNlt l t = Variable(Local_variable(l, Component_type(t)))
let eNlp l p = Variable(Local_variable(l, Port(p)))
let eNlk l k = Variable(Local_variable(l, Package(k)))

let eB p tp tr = Variable(Binding_variable(p,tp,tr))
let eR l r = Variable(Local_repository_variable(l,r))
let eO l o = Variable(Local_resource_variable(l,o))




(*******************************************)
(** 2. Universe Translation                *) (* using naming conventions from the paper *)
(*******************************************)

(* flat model *)
let require u_dp ur up get_component_type = 
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
        ((get_require_arity (get_component_type tr) p) *~ (eNt tr))
          <=~ (sum (Data_model.Component_id_set.fold (fun tp res -> (eB p tp tr)::res) (up p) [])))::res
    ) (ur p) res
  ) u_dp []

let provide u_dp up ur get_component_type = 
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tp res -> (
          ((get_provide_arity (get_component_type tp) p) *~ (eNt tp))
          >=~ (sum (Data_model.Component_id_set.fold (fun tr res -> (eB p tp tr)::res) (ur p) [])))::res
    ) (up p) res
  ) u_dp []

let binding u_dp ur up = 
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res ->
      Data_model.Component_type_id_set.fold (fun tp res ->
        ((eB p tp tr) <=~ ((eNt tr) *~ (eNt tr)))::res
      ) (up p) res
    ) (ur p) res
  ) u_dp []


let conflict u_dp uc get_component_type =
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNt t) >=~ (constant 1)) =>~~ ((eNp p) =~ (get_provide_arity (get_component_type t) p)))::res
    ) (uc p) res
  ) u_dp []

(* location val *)
let location_component_type u_dt c_l =
  Data_model.Component_type_id_set.fold (fun t res ->
    ((eNt t) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlt l t)::res) c_l [])))::res
  ) u_dt []
  
let location_package u_dk c_l =
  Data_model.Package_id_set.fold (fun k res ->
    ((eNk k) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlk l k)::res) c_l [])))::res
  ) u_dk []

let location_port u_dp c_l =
  Data_model.Port_id_set.fold (fun p res ->
    ((eNp p) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlp l p)::res) c_l [])))::res
  ) u_dp []

let location_port_equation u_dp c_l up get_component_type = 
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Location_id_set.fold (fun l res ->
      ((eNlp l p) =~ (sum (Data_model.Component_type_id_set.fold
                       (fun t res -> ((get_provide_arity (get_component_type t) p) *~ (eNlt l t))::res) (up p) [])))::res
    ) c_l res
  ) u_dp []


  (* Repositories *)
let repository_unique c_l u_dr =
  Data_model.Location_id_set.fold (fun l res ->
    ((constant 1) =~ (sum (Data_model.Repository_id_set.fold (fun r res -> (eR l r)::res) u_dr [])))::res
  ) c_l []

let repository_package c_l u_dr u_dk get_packages =
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Repository_id_set.fold (fun r res ->
      (((eR l r) =~ (constant 1)) =>~~
        (And (Data_model.Package_id_set.fold (fun k res -> ((eNlk l k) =~ (constant 0))::res) (Data_model.Package_id_set.diff u_dk (get_packages r)) [])))::res
    ) u_dr res
  ) c_l []

  (* Package dependencies *)
let component_type_implementation c_l u_dt u_i =
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNlt l t) >=~ (constant 1)) =>~~ ((sum (Data_model.Package_id_set.fold (fun k res -> (eNlk l k)::res) (u_i t) [])) >=~ (constant 1)))::res
    ) u_dt res
  ) c_l []

let package_dependency c_l u_dk get_package =
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set_set.fold (fun ks res ->
        ((eNlk l k1) <=~ (sum (Data_model.Package_id_set.fold (fun k2 res -> (eNlk l k2)::res) ks [])))::res
      ) ((get_package k1)#depend) res
    ) u_dk res
  ) c_l []

let package_conflict c_l u_dk get_package =
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set.fold (fun k2 res ->
        (((eNlk l k1) +~ (eNlk l k2)) <=~ (constant 1))::res
      ) ((get_package k1)#conflict) res
    ) u_dk res
  ) c_l []


  (* Resource consumptions *)
let resource_consumption c_l resources u_dt u_dk get_component_type get_package =
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Resource_id_set.fold (fun o res ->
      ((sum (Data_model.Package_id_set.fold (fun k res -> ((get_consume (get_package k) o) *~ (eNlk l k))::res) u_dk
              (Data_model.Component_type_id_set.fold (fun t res -> ((get_consume (get_component_type t) o) *~ (eNlt l t))::res) u_dt []))) <=~ (eO l o))::res
    ) resources res
  ) c_l []

  (* Deprecated packages and component types *)
let deprecated_component_types_and_packages c_l =
  Data_model.Location_id_set.fold (fun l res ->
    ((eNlt l Data_model.deprecated_component_type_id) =~ (constant 0))::((eNlk l Data_model.deprecated_package_id) =~ (constant 0))::res
  ) c_l []

let universe_full () =
  let f (universe: Data_model.universe) configuration resources=
    Data_state.constraint_universe_component_type_require        := require universe#u_dp universe#ur universe#up universe#get_component_type;
    Data_state.constraint_universe_component_type_provide        := provide universe#u_dp universe#up universe#ur universe#get_component_type;
    Data_state.constraint_universe_component_type_conflict       := conflict universe#u_dp universe#uc universe#get_component_type;
    Data_state.constraint_universe_component_type_implementation := component_type_implementation configuration#c_l universe#u_dt universe#u_i;
    Data_state.constraint_universe_binding_unicity               := binding universe#u_dp universe#ur universe#up;
    Data_state.constraint_universe_location_component_type       := location_component_type universe#u_dt configuration#c_l;
    Data_state.constraint_universe_location_package              := location_package universe#u_dk configuration#c_l;
    Data_state.constraint_universe_location_port                 := location_port universe#u_dp configuration#c_l;
    Data_state.constraint_universe_definition_port               := location_port_equation universe#u_dp configuration#c_l universe#up universe#get_component_type;
    Data_state.constraint_universe_repository_unicity            := repository_unique configuration#c_l universe#u_dr;
    Data_state.constraint_universe_repository_package            :=
      repository_package configuration#c_l universe#u_dr universe#u_dk (fun r -> (universe#get_repository r)#package_ids);
    Data_state.constraint_universe_package_dependency            := package_dependency configuration#c_l universe#u_dk universe#get_package;
    Data_state.constraint_universe_package_conflict              := package_conflict configuration#c_l universe#u_dk universe#get_package;
    Data_state.constraint_universe_resource_consumption          :=
      resource_consumption configuration#c_l resources#resource_ids universe#u_dt universe#u_dk universe#get_component_type universe#get_package;
    Data_state.constraint_universe_deprecated_element            := deprecated_component_types_and_packages configuration#c_l
  in match (!Data_state.universe_full, !Data_state.initial_configuration_full, !Data_state.resources_full) with
    | (Some(u), Some(c), Some(r)) -> f u c r
    | _ -> ()


(*******************************************)
(** 3. Specification Translation           *) (* using naming conventions from the paper *)
(*******************************************)

let spec_variable_name v = Variable(Simple_variable(v))
let spec_const = constant

let spec_local_element l e = match e with
  | Data_model.Spec_local_element_package (package_id) -> eNlk l package_id
  | Data_model.Spec_local_element_component_type (component_type_id) -> eNlt l component_type_id
  | Data_model.Spec_local_element_port (port_id) -> eNlp l port_id

let rec spec_local_expr l e = match e with
  | Data_model.Spec_local_expr_var v -> spec_variable_name v
  | Data_model.Spec_local_expr_const c -> spec_const c
  | Data_model.Spec_local_expr_arity e -> spec_local_element l e
  | Data_model.Spec_local_expr_add (e1, e2) -> (spec_local_expr l e1) +~ (spec_local_expr l e2)
  | Data_model.Spec_local_expr_sub (e1, e2) -> (spec_local_expr l e1) -~ (spec_local_expr l e2)
  | Data_model.Spec_local_expr_mul (e1, e2) -> (spec_const e1) *~ (spec_local_expr l e2)

let spec_op o = match o with
  | Data_model.Lt  -> (<~) | Data_model.LEq -> (<=~) | Data_model.Eq  -> (=~)
  | Data_model.GEq -> (>=~) | Data_model.Gt  -> (>~) | Data_model.NEq -> (<>~)

let rec local_specification l s = match s with
  | Data_model.Spec_local_true -> True
  | Data_model.Spec_local_op (e1, op, e2) -> (spec_op op) (spec_local_expr l e1) (spec_local_expr l e2)
  | Data_model.Spec_local_and (s1, s2) -> (local_specification l s1) &&~~ (local_specification l s2)
  | Data_model.Spec_local_or (s1, s2) -> (local_specification l s1) ||~~ (local_specification l s2)
  | Data_model.Spec_local_impl (s1, s2) -> (local_specification l s1) =>~~ (local_specification l s2)
  | Data_model.Spec_local_not (s') -> not (local_specification l s')

let spec_resource_constraint l co = List.fold_left (fun res (o, op, i) -> ((spec_op op) (eO l o) (constant i))::res) [] co
let spec_repository_constraint l cr = (sum (List.map (fun r -> eR l r) cr)) =~ (constant 1)

let spec_element location_ids e = match e with
  | Data_model.Spec_element_package (package_id) -> eNk package_id
  | Data_model.Spec_element_component_type (component_type_id) -> eNt component_type_id
  | Data_model.Spec_element_port (port_id) -> eNp port_id
  | Data_model.Spec_element_location (co, cr, ls) -> sum (Data_model.Location_id_set.fold
     (fun l res -> (reify (And((local_specification l ls)::(spec_repository_constraint l cr)::(spec_resource_constraint l co))))::res ) location_ids [])

let rec spec_expr location_ids e = match e with
  | Data_model.Spec_expr_var v -> spec_variable_name v
  | Data_model.Spec_expr_const c -> spec_const c
  | Data_model.Spec_expr_arity e -> spec_element location_ids e
  | Data_model.Spec_expr_add (e1, e2) -> (spec_expr location_ids e1) +~ (spec_expr location_ids e2)
  | Data_model.Spec_expr_sub (e1, e2) -> (spec_expr location_ids e1) -~ (spec_expr location_ids e2)
  | Data_model.Spec_expr_mul (e1, e2) -> (spec_const e1) *~ (spec_expr location_ids e2)

let rec specification location_ids s = match s with
  | Data_model.Spec_true -> True
  | Data_model.Spec_op (e1, op, e2) -> (spec_op op) (spec_expr location_ids e1) (spec_expr location_ids e2)
  | Data_model.Spec_and (s1, s2) -> (specification location_ids s1) &&~~ (specification location_ids s2)
  | Data_model.Spec_or  (s1, s2) -> (specification location_ids s1) ||~~ (specification location_ids s2)
  | Data_model.Spec_impl (s1, s2) -> (specification location_ids s1) =>~~ (specification location_ids s2)
  | Data_model.Spec_not (s') -> not (specification location_ids s')


let specification_full () = match (!Data_state.specification_full, !Data_state.initial_configuration_full) with
  | (Some(s), Some(c)) -> Data_state.constraint_specification_full := Some(specification c#get_location_ids s)
  | _  -> ()


(*******************************************)
(** 4. Configuration Translation           *) (* using naming conventions from the paper *)
(*******************************************)

let configuration resources c_l get_location = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Resource_id_set.fold (fun o res ->
      ((eO l o) =~ (constant ((get_location l)#provide_resources o)))::res
    ) resources res
  ) c_l []

let configuration_full () = match (!Data_state.initial_configuration_full, !Data_state.resources_full) with
    | (Some(c), Some(r)) -> Data_state.constraint_configuration_full := configuration r#resource_ids c#get_location_ids c#get_location
    | _ -> ()


(*******************************************)
(** 5. Optimisation function Translation   *)
(*******************************************)

(* preliminary definitions for costs *)
let cost_all_components u_dt = (Data_model.Component_type_id_set.fold (fun t res -> (eNt t)::res) u_dt [])
let cost_all_packages   u_dk = (Data_model.Package_id_set.fold (fun k res -> (eNk k)::res) u_dk [])

let cost_local_components l u_dt = (Data_model.Component_type_id_set.fold (fun t res -> (eNlt l t)::res) u_dt [])
let cost_local_packages   l u_dk = (Data_model.Package_id_set.fold (fun k res -> (eNlk l k)::res) u_dk [])

type used_or_free_location = Used_locations | Free_locations
let cost_locations switch c_l u_dt u_dk count_components_only =
  Data_model.Location_id_set.fold (fun l res ->
    (reify ((if switch = Used_locations then (>~) else (=~) ) (sum (if count_components_only then cost_local_components l u_dt
              else List.rev_append (cost_local_components l u_dt) (cost_local_packages l u_dk))) (constant 0)))::res
  ) c_l []

let cost_used_locations = cost_locations Used_locations
let cost_free_locations = cost_locations Free_locations

let cost_difference_components c_l u_dt get_local_component = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (abs((eNlt l t) -~ (constant (Data_model.Component_id_set.cardinal (get_local_component l t))) ))::res
    ) u_dt res) c_l []

let cost_difference_packages c_l u_dk get_local_package = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k res ->
      (if get_local_package l k then abs((eNlk l k) -~ (constant 1)) else (eNlk l k))::res
    ) u_dk res) c_l []

(* translation functions *)
let compact_slow c_l u_dt u_dk =
  Lexicographic ([
    Minimize (sum (cost_used_locations c_l u_dt u_dk false)); (* First minimize the number of used locations *)
    Minimize (sum (cost_all_components u_dt));                (* then minimize the number of components *)
    Minimize (sum (cost_all_packages u_dk)) ])                (* finally minimize the number of packages. (so we do not have useless packages) *)
let compact_fast c_l u_dt u_dk = Minimize (sum (List.rev_append (List.rev_append (cost_all_components u_dt) (cost_used_locations c_l u_dt u_dk false))
                                                                        (cost_all_packages u_dk)))
let spread_slow c_l u_dt u_dk =
  Lexicographic ([
    Minimize (sum (cost_all_components u_dt));               (* First minimize the number of components *)
    Maximize (sum (cost_used_locations c_l u_dt u_dk true)); (* then maximize the number of used locations, (counting only locations with at least one component) *)
    Minimize (sum (cost_all_packages u_dk)) ])               (* finally minimize the number of packages. *)
let spread_fast c_l u_dt u_dk = Maximize ((sum (cost_used_locations c_l u_dt u_dk true)) -~ (sum (cost_all_components u_dt)))

let conservative_slow c_l u_dt u_dk get_local_component get_local_package =
  Lexicographic ([
    Minimize (sum (cost_difference_components c_l u_dt get_local_component)); (* First minimize the number of changed components *)
    Minimize (sum (cost_used_locations c_l u_dt u_dk false));                 (* then minimize the number of used locations *)
    Minimize (sum (cost_difference_packages c_l u_dk get_local_package)) ])   (* finally minimize the number of changed packages *)

(* set the optimization function in Data_state *)

let optimization_function () = match !Data_state.optimization_function with
  | None -> () | Some(o) -> match (!Data_state.universe_full, !Data_state.initial_configuration_full) with
    | (Some(u), Some(c)) -> (let (c_l, u_dt, u_dk, get_local_component, get_local_package) = (c#c_l, u#u_dt, u#u_dk, c#get_local_component, c#get_local_package) in
      Data_state.constraint_optimization_function := Some(match o with
      | Data_model.Optimization_function_simple       -> Minimize (sum (cost_all_components u_dt))
      | Data_model.Optimization_function_compact      -> compact_slow c_l u_dt u_dk
      | Data_model.Optimization_function_conservative -> conservative_slow c_l u_dt u_dk get_local_component get_local_package
      | Data_model.Optimization_function_spread       -> spread_slow c_l u_dt u_dk
      | Data_model.Optimization_function_none         -> Lexicographic([])))
    | _ -> ()






(*******************************************)
(** 6. Very Simple Bounds Definition       *)
(*******************************************)

let basic_bounds_function v = (** this function gives the basic bounds of every variable: [min = 0] and [max = \infty] except for packages and repositories *)
  let big   = { min = value 0; max = infinite_value } in
  let small = { min = value 0; max = value 1 } in match v with
  | Simple_variable         _ -> big
  | Global_variable         _ -> big
  | Local_variable     (_, e) -> (match e with | Package(_) -> small | _ -> big)
  | Binding_variable        _ -> big
  | Local_repository_variable _ -> small
  | Local_resource_variable   _ -> big

let basic_bounds () = Data_state.constraint_variable_bounds := Some(basic_bounds_function)

  (* Optimization function *)


(*
  part universe location_name_set resource_name_set u = 
  val configuration resource_name_set c = 
  val specification s = 
  val optimization_function o =

  val universe_flat universe =
*)

