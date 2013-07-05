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
    - datatypes/Data_common (for category computation)
    - datatypes/Data_model
    - datatypes/Data_constraint
*)

open Data_model
open Data_constraint

include Location_id_set_set


let domain ss = fold (fun s res -> Location_id_set.union s res) ss Location_id_set.empty

(*/************************************)
(*| category computation part *)



let compare_components (u: universe) (c: configuration) l1 l2 =
  Component_type_id_set.fold (fun t b -> b &&
     ((Component_id_set.cardinal (c#get_local_component l1 t)) = ((Component_id_set.cardinal(c#get_local_component l2 t)))) ) u#get_component_type_ids true
let compare_repositories l1 l2 = (l1#repository = l2#repository)
let compare_packages l1 l2 = ((Data_model.Package_id_set.compare (l1#packages_installed) (l2#packages_installed)) = 0)
let compare_resources resources l1 l2 = Data_model.Resource_id_set.fold (fun r b -> b && ((l1#provide_resources r) = (l2#provide_resources r))) resources#resource_ids true
let compare_cost l1 l2 = (Data_model.Location_cost.compare l1#cost l2#cost) = 0

let compare_simple resources l1 l2 = (compare_resources resources l1 l2) && (compare_cost l1 l2)

let compare_full resources (u:universe) c l1 l2 = let (l1',l2') = (c#get_location l1, c#get_location l2) in
  (compare_simple resources l1' l2') && (compare_repositories l1' l2') && (compare_packages l1' l2') && (compare_components u c l1 l2)

module Location_categories = Data_common.Set.EquivalenceClass(Data_model.Location_id_set)(Data_model.Location_id_set_set)

(* generate the categories only considering resources, for optimizations like [compact] or [spread] *)
let resource_categories resources c = Location_categories.compute (fun l1 l2 -> compare_simple resources (c#get_location l1) (c#get_location l2)) c#get_location_ids
(* generate the categories for the [conservative] optimization function *)
let full_categories resources u c =  Location_categories.compute (compare_full resources u c) c#get_location_ids

(*/************************************)
(*| constraint computation part *)

let elements_of_location u l = sum ((List.map (fun t -> Variable(Local_variable(l, Component_type(t)))) (Component_type_id_set.elements u#get_component_type_ids))
                                  @ (List.map (fun k -> Variable(Local_variable(l, Package(k)))) (Package_id_set.elements u#get_package_ids)))
let constraint_of_category u s =
  let rec f ls =  match ls with
  | [] -> [] | [l] -> []
  | l1::l2::ls' -> ( (elements_of_location u l1) <=~ (elements_of_location u l2) ) ::(f (l2::ls')) in f (Location_id_set.elements s)

let constraint_of u ss = conj (Location_id_set_set.fold (fun s res -> res @ (constraint_of_category u s)) ss [])


(*/************************************)
(*| main part *)

let categories = ref Data_model.Location_id_set_set.empty

let generate_categories () = match (!Data_state.resources_full, !Data_state.universe_full, !Data_state.initial_configuration_full, !Data_state.optimization_function) with
  | (Some(r), Some(u), Some(c), Some(f)) -> (match f with
    | Data_model.Optimization_function_conservative -> categories := full_categories r u c
    | _ -> categories := resource_categories r c)
  | _ -> ()

let generate_constraint () = match !Data_state.universe_full with
  | Some(u) -> constraint_of u !categories
  | _ -> True


