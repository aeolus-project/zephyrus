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
    - atd/Json_binpacking_t
    - atd/Json_binpacking_j
*)

open Data_common
open Data_model


(* Basic conversion functions. *)
let item_name_of_component_type_id (component_type_id      : component_type_id)      : Json_binpacking_t.item_name = Name_of.component_type_id component_type_id
let dimension_of_resource_id       (resource_id            : resource_id)            : Json_binpacking_t.dimension = Name_of.resource_id       resource_id
let bin_cost_of_location_cost      (location_cost          : location_cost)          : Json_binpacking_t.bin_cost  = location_cost
let size_of_resource_consume_arity (resource_consume_arity : resource_consume_arity) : Json_binpacking_t.size      = resource_consume_arity
let size_of_resource_provide_arity (resource_provide_arity : resource_provide_arity) : Json_binpacking_t.size      = resource_provide_arity

(* Main convertion function. *)
let convert_configuration (u : universe) (c : configuration) =
  
  let items = 
    (* Get all the available component types. *)
    let component_type_ids = Component_type_id_set.elements u#get_component_type_ids in

    (* Create an item from each component type. *)
    List.map (fun component_type_id ->
      let component_type = u#get_component_type component_type_id in

      (* Name of the item. 
         Is equal to the name of the component type. *)
      let name = item_name_of_component_type_id component_type_id in

      (* Size of the item in different dimensions. 
         Corresponds to the resource consumption of the component type. *)
      let sizes = 
        (* let consume_domain = Resource_id_set.elements component_type#consume_domain in *)
        let consume_domain = Resource_id_set.elements u#get_resource_ids in
        List.map (fun resource_id ->
          let dimension = dimension_of_resource_id                               resource_id in
          let size      = size_of_resource_consume_arity (component_type#consume resource_id) in
          (dimension, size)
        ) consume_domain in

      (* Arity: how many of these items are there.
         Corresponds to the number of components of this type in the configuration. *)
      let arity =
        Component_id_set.cardinal (
          Component_id_set.filter (fun component_id ->
            let component = c#get_component component_id in
            component#typ = component_type_id
          ) c#get_component_ids) in

      (* The item: *)
      {
        Json_binpacking_t.item_name  = name;
        Json_binpacking_t.item_sizes = sizes;
        Json_binpacking_t.item_arity = arity;
      }
    ) component_type_ids
  in
  
  let bins = 
    (* Prepare location categories, their arity and their representants. *)
    let location_categories : Location_id_set.t list = 
      Location_id_set_set.elements (Location_categories.resource_categories u c) in

    (* Create a bin from each location category. *)
    List.mapi (fun category_number location_category ->

      (* Category representant.
         All the locations in the category are the same (they only have different names)
         so we can take any single one. *)
      let representant_location_id = Location_id_set.choose location_category in
      let location = c#get_location representant_location_id in

      (* Name of the bin. 
         We generate a unique name corresponding to each location category. *)
      let name = Printf.sprintf "Location_category_%d" (category_number + 1) in

      (* Size of the bin in different dimensions. 
         Corresponds to the resources provided by the location. *)
      let sizes = 
        (* let provide_domain = Resource_id_set.elements location#provide_resources_domain in *)
        let provide_domain = Resource_id_set.elements u#get_resource_ids in
        List.map (fun resource_id ->
          let dimension = dimension_of_resource_id                                   resource_id in
          let size      = size_of_resource_provide_arity (location#provide_resources resource_id) in
          (dimension, size)
        ) provide_domain in

      (* Cost of using the bin. 
         Is equal to the cost of using the location. *)
      let cost = bin_cost_of_location_cost location#cost in

      (* Arity: how many of these bins are available.
         Corresponds to the number of locations in the location category. *)
      let arity = Location_id_set.cardinal location_category in
      
      (* The bin: *)
      {
        Json_binpacking_t.bin_name  = name;
        Json_binpacking_t.bin_sizes = sizes;
        Json_binpacking_t.bin_cost  = cost;
        Json_binpacking_t.bin_arity = arity;
      }

    ) location_categories
  in

  let incompatibilities = 
      Repository_id_set.map_to_list (fun repository_id ->
        let repository_name   : Json_binpacking_t.repository_name   = Name_of.repository_id repository_id in
        let incompatibilities : Json_binpacking_t.incompatibilities = 
          let incompatibilities' : component_type_id list list = 
            let incompatibilities_set : Component_type_id_set_set.t = Incompatibilities_of.repository u repository_id in
            Component_type_id_set_set.map_to_list Component_type_id_set.elements incompatibilities_set in
          List.map (List.map item_name_of_component_type_id) incompatibilities' in
        (repository_name, incompatibilities)
      ) u#get_repository_ids

  in

  (* The binpacking problem: *)
  {
    Json_binpacking_t.binpacking_problem_items             = items;
    Json_binpacking_t.binpacking_problem_bins              = bins;
    Json_binpacking_t.binpacking_problem_incompatibilities = incompatibilities;
  }


let configuration (u : universe) (c : configuration) = 
  Yojson.Safe.prettify (Json_binpacking_j.string_of_binpacking_problem (convert_configuration u c))

module X = Incompatibilities_of