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

module Core = struct
  let get_provide_arity c p = Constant(value_of_provide_arity (Functions_model.Core.get_provide_arity c p))
  let get_require_arity c p = Constant(value_of_require_arity (Functions_model.Core.get_require_arity c p))

  let eNt t = Variable(Global_variable(Component_type(t)))
  let eNp p = Variable(Global_variable(Port(p)))
  let eNk k = Variable(Global_variable(Package(k)))
  let eNlt l t = Variable(Local_variable(l, Component_type(t)))
  let eNlp l p = Variable(Local_variable(l, Port(p)))
  let eNlk l k = Variable(Local_variable(l, Package(k)))

  let eB p tp tr = Variable(Binding_variable(p,tp,tr))

  let eR l r = Variable(Local_repository_variable(l,r))
end


module Improved = struct

  (* flat model *)
  let require u_dp ur get_component_type up = 
    Data_model.Port_name_set.fold (fun p res ->
      Data_model.Component_type_name_set.fold (fun tr res ->
        And(res, 
          Arith_constraint(
            Times((Core.get_provide_arity (get_component_type(tr)) p), Core.eNt tr),
            LEq,
            Data_model.Component_name_set.fold (fun tp res -> Plus(res, Core.eB p tp tr)) (up p) (Constant(value 0))
          )
        )
      ) (ur p) res
    ) u_dp Data_constraint.True

  let provide u_dp up get_component_type ur = 
    Data_model.Port_name_set.fold (fun p res ->
      Data_model.Component_type_name_set.fold (fun tp res ->
        And(res, 
          Arith_constraint(
            Times((Core.get_require_arity (get_component_type(tp)) p), Core.eNt tp),
            GEq,
            Data_model.Component_name_set.fold (fun tr res -> Plus(res, Core.eB p tp tr)) (ur p) (Constant(value 0))
          )
        )
      ) (up p) res
    ) u_dp Data_constraint.True

  let binding u_dp ur up = 
    Data_model.Port_name_set.fold (fun p res ->
      Data_model.Component_type_name_set.fold (fun tr res ->
        Data_model.Component_type_name_set.fold (fun tp res ->
          And(res, Arith_constraint(Variable(Binding_variable(p,tp,tr)), LEq, Times(Core.eNt tr, Core.eNt tr)))
        ) (up p) res
      ) (ur p) res
    ) u_dp Data_constraint.True


  let conflict u_dp uc get_component_type =
    Data_model.Port_name_set.fold (fun p res ->
      Data_model.Component_type_name_set.fold (fun t res ->
        And(res,
          Implies(Arith_constraint(Core.eNt t, GEq, Constant(value 1)), Arith_constraint(Core.eNp p, Eq, (Core.get_provide_arity (get_component_type(t)) p)))
        )
      ) (uc p) res
    ) u_dp Data_constraint.True


  (* location val *)
  let location_component_type u_dt c_l =
    Data_model.Component_type_name_set.fold (fun t res ->
      And(res, Arith_constraint(Core.eNt t, Eq,
        Data_model.Location_name_set.fold (fun l res ->
          Plus(res, Core.eNlt l t)
        ) c_l (Constant(value 0)))
      )
    ) u_dt Data_constraint.True
    
  let location_package u_dk c_l =
    Data_model.Package_name_set.fold (fun k res ->
      And(res, Arith_constraint(Core.eNk k, Eq,
        Data_model.Location_name_set.fold (fun l res ->
          Plus(res, Core.eNlk l k)
        ) c_l (Constant(value 0)))
      )
    ) u_dk Data_constraint.True

  let location_port u_dp c_l =
    Data_model.Port_name_set.fold (fun p res ->
      And(res, Arith_constraint(Core.eNp p, Eq,
        Data_model.Location_name_set.fold (fun l res ->
          Plus(res, Core.eNlp l p)
        ) c_l (Constant(value 0)))
      )
    ) u_dp Data_constraint.True

  let location_port_equation u_dp c_l up get_component_type = 
    Data_model.Port_name_set.fold (fun p res ->
      Data_model.Location_name_set.fold (fun l res ->
        And(res, Arith_constraint(Core.eNlp l p, Eq, 
          Data_model.Component_type_name_set.fold (fun t res ->
            Plus(res, Times(Core.get_provide_arity (get_component_type t) p, Core.eNlt l t))
          ) (up p) (Constant(value 0)))
        )
      ) c_l res
    ) u_dp Data_constraint.True


  (* Repositories *)
  let repository_unique c_l u_dr =
    Data_model.Location_name_set.fold (fun l res ->
      And(res, Arith_constraint(Constant(value 1), Eq, Data_model.Repository_name_set.fold (fun r res -> Plus(res, Core.eR l r)) u_dr (Constant(value 0))))
    ) c_l Data_constraint.True

  let repository_package c_l u_dr u_dk get_packages =
    Data_model.Location_name_set.fold (fun l res ->
      Data_model.Repository_name_set.fold (fun r res ->
        And(res, Implies(Arith_constraint(Core.eR l r, Eq, Constant(value 1)),
          Data_model.Package_name_set.fold (fun k res -> And(res, Arith_constraint(Core.eNlk l k, Eq, Constant(value 0))))
            (Data_model.Package_name_set.diff u_dk (get_packages r)) Data_constraint.True)
        )
      ) u_dr res
    ) c_l Data_constraint.True

  (* Package dependencies *)
  (* TODO *)

  (* Resource consumptions *)
  (* TODO *)

  (* Deprecated packages and component types *)
  (* TODO *)




  (* bounds *)
  let basic_bounds v = (** this function gives the basic bounds of every variable: [min = 0] and [max = \infty] except for packages and repositories *)
    let big   = { min = value 0; max = infinite_value } in
    let small = { min = value 0; max = value 1 } in match v with
  | Simple_variable         _ -> big
  | Global_variable         _ -> big
  | Local_variable     (_, e) -> (match e with | Package(_) -> small | _ -> big)
  | Binding_variable        _ -> big
  | Local_repository_variable _ -> small
  | Local_resource_variable   _ -> big


  (* Optimization function *)


(*
  part universe location_name_set resource_name_set u = 
  val configuration resource_name_set c = 
  val specification s = 
  val optimization_function o =

  val universe_flat universe =
*)
end
