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
    - datatypes/Data_helper
    - atd/Json_binpacking_j
*)

open Data_model

(* Helpers:*)

(* Extend the List module with mapi function (it's not necessary after OCaml 4.00.0). *)
module List_global_from_stdlib = List

module List : sig
  include module type of List_global_from_stdlib
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
end = struct
  include List_global_from_stdlib

  let rec indices l current_index =
    match l with
    | []   -> []
    | _::t -> current_index :: (indices t (current_index + 1))

  let mapi : (int -> 'a -> 'b) -> 'a list -> 'b list = 
    fun f l -> List.map2 f (indices l 0) l  
end


(* Basic conversion functions. *)
let item_name_of_component_type_id (component_type_id : component_type_id) : Json_binpacking_t.item_name     = Name_of.component_type_id component_type_id
let resource_name_of_resource_id   (resource_id       : resource_id)       : Json_binpacking_t.resource_name = Name_of.resource_id       resource_id
let bin_cost_of_location_cost      (location_cost     : location_cost)     : Json_binpacking_t.bin_cost      = location_cost

let convert_resource_consume_arity (resource_consume_arity : resource_consume_arity) : Json_binpacking_t.resource_consume_arity = resource_consume_arity
let convert_resource_provide_arity (resource_provide_arity : resource_provide_arity) : Json_binpacking_t.resource_provide_arity = resource_provide_arity

(* Main convertion function. *)
let convert_configuration (u : universe) (c : configuration) =
  
  let items = 
    (* Get all the available component types. *)
    let component_type_ids = Component_type_id_set.elements u#get_component_type_ids in

    (* Create an item from each component type. *)
    List.map (fun component_type_id ->
      let component_type = u#get_component_type component_type_id in

      (* Name. *)
      let name = item_name_of_component_type_id component_type_id in

      (* Resource consumption. *)
      let consume = 
        let consume_domain = Resource_id_set.elements component_type#consume_domain in
        List.map (fun resource_id ->
          let resource_name          = resource_name_of_resource_id                     resource_id in
          let resource_consume_arity = convert_resource_consume_arity (component_type#consume resource_id) in
          (resource_name, resource_consume_arity)
        ) consume_domain in

      (* Arity. *)
      let arity =
        Component_id_set.cardinal (
          Component_id_set.filter (fun component_id ->
            let component = c#get_component component_id in
            component#typ = component_type_id
          ) c#get_component_ids) in

      (* The item: *)
      {
        Json_binpacking_t.item_name    = name;
        Json_binpacking_t.item_consume = consume;
        Json_binpacking_t.item_arity   = arity;
      }
    ) component_type_ids
  in
  
  let bins = 
    (* Prepare location categories, their arity and their representants. *)
    let location_categories : Location_id_set.t list = Location_id_set_set.elements (Location_categories.resource_categories u c) in

    (* Create a bin from each location category. *)
    List.mapi (fun category_number location_category ->
      (* Category representant and arity. *)
      let representant_location_id = Location_id_set.choose   location_category in
      let arity                    = Location_id_set.cardinal location_category in
      
      let location = c#get_location representant_location_id in

      (* Name. *)
      let name = Printf.sprintf "Location_category_%d" (category_number + 1) in

      (* Resources provided. *)
      let provide = 
        let provide_domain = Resource_id_set.elements location#provide_resources_domain in
        List.map (fun resource_id ->
          let resource_name          = resource_name_of_resource_id                     resource_id in
          let resource_provide_arity = convert_resource_provide_arity (location#provide_resources resource_id) in
          (resource_name, resource_provide_arity)
        ) provide_domain in

      (* Cost. *)
      let cost = bin_cost_of_location_cost location#cost in

      (* The bin: *)
      {
        Json_binpacking_t.bin_name    = name;
        Json_binpacking_t.bin_provide = provide;
        Json_binpacking_t.bin_cost    = cost;
        Json_binpacking_t.bin_arity   = arity;
      }

    ) location_categories
  in

  (* The binpacking problem: *)
  {
    Json_binpacking_t.binpacking_problem_items = items;
    Json_binpacking_t.binpacking_problem_bins  = bins;
  }

let configuration (u : universe) (c : configuration) = 
  Yojson.Safe.prettify (Json_binpacking_j.string_of_binpacking_problem (convert_configuration u c))