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


(* compute the domain for a set of location categories *)
let domain ss = fold (fun s res -> Location_id_set.union s res) ss Location_id_set.empty

(*/************************************)
(*| category computation part                                                                                                                         *)
(*|  this part computes the set L/R where L is the set of location and R is an equivalence relation based on the characteristics of the locations.    *)
(*|  currently, we have two relation R:                                                                                                               *)
(*|   - when locations are characterized only with the amount of provided resources (function resource_categories)                                    *)
(*|   - when, in addition of the amount of provided resources, also the installed repository, packages and component count (function full_categories) *)

(* bunch of functions to compare two locations, with a specific criteria *)
let compare_components (u: universe) (c: configuration) l1 l2 =
  Component_type_id_set.fold (fun t b -> b &&
     ((Component_id_set.cardinal (c#get_local_component l1 t)) = ((Component_id_set.cardinal(c#get_local_component l2 t)))) ) u#get_component_type_ids true
let compare_repositories l1 l2 = (l1#repository = l2#repository)
let compare_packages l1 l2 = ((Data_model.Package_id_set.compare (l1#packages_installed) (l2#packages_installed)) = 0)
let compare_resources resource_ids l1 l2 = Data_model.Resource_id_set.fold (fun r b -> b && ((l1#provide_resources r) = (l2#provide_resources r))) resource_ids true
let compare_cost l1 l2 = (Data_model.Location_cost.compare l1#cost l2#cost) = 0

let compare_simple resource_ids l1 l2 = (compare_resources resource_ids l1 l2) && (compare_cost l1 l2)

let compare_full (u:universe) c l1 l2 = let (l1',l2') = (c#get_location l1, c#get_location l2) in
  (compare_simple u#get_resource_ids l1' l2') && (compare_repositories l1' l2') && (compare_packages l1' l2') && (compare_components u c l1 l2)

module Location_categories = Data_common.Set.EquivalenceClass(Data_model.Location_id_set)(Data_model.Location_id_set_set)

(* generate the categories only considering resources, for optimizations like [compact] or [spread] *)
let resource_categories u c = Location_categories.compute (fun l1 l2 -> compare_simple u#get_resource_ids (c#get_location l1) (c#get_location l2)) c#get_location_ids
(* generate the categories for the [conservative] optimization function *)
let full_categories u c =  Location_categories.compute (compare_full u c) c#get_location_ids

(*/************************************)
(*| constraint computation part                                                                                                      *)
(*| this part computes a constraint that break symmetry of locations for the solver:                                                 *)
(*|   the number of elements installed on the location $i$ is greater or equal to the number of elements installed on location $i+1$ *)


let elements_of_location (no_packages : bool) u l = 
  let components = (List.map (fun t -> Variable(Local_variable(l, Component_type(t)))) (Component_type_id_set.elements u#get_component_type_ids)) in
  let packages   = (List.map (fun k -> Variable(Local_variable(l, Package(k)))       ) (Package_id_set.elements        u#get_package_ids)) in
  if no_packages 
  then sum  components
  else sum (components @ packages)

let constraint_of_category (no_packages : bool) u s =
  let rec f ls =  match ls with
  | []  -> [] 
  | [l] -> []
  | l1::l2::ls' -> ( (elements_of_location no_packages u l1) <=~ (elements_of_location no_packages u l2) ) ::(f (l2::ls')) in f (Location_id_set.elements s)

let constraint_of (no_packages : bool) u ss = conj (Location_id_set_set.fold (fun s res -> res @ (constraint_of_category no_packages u s)) ss [])


(*/************************************)
(*| main part *)

let generate_categories universe initial_configuration optimization_function = 
  match (universe, initial_configuration, optimization_function) with
  | (Some(u), Some(c), Some(f)) -> 
    (match f with
    | Data_model.Optimization_function_conservative -> full_categories     u c
    | _                                             -> resource_categories u c)
  | _ -> empty

let generate_constraint (no_packages : bool) universe categories = 
  match universe with
  | Some(u) -> constraint_of no_packages u categories
  | _ -> True
