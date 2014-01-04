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
    let location_categories : Location_id_set.t list = Location_id_set_set.elements (Location_categories.resource_categories u c) in

    (* Create a bin from each location category. *)
    List.mapi (fun category_number location_category ->

      (* Category representant.
         All the locations in the category are the same (they only have different names)
         so we can take any single one. *)
      let representant_location_id = Location_id_set.choose   location_category in
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

  (* The binpacking problem: *)
  {
    Json_binpacking_t.binpacking_problem_items = items;
    Json_binpacking_t.binpacking_problem_bins  = bins;
  }


type real_or_pseudo_package =
  | Real_package   of package_id
  | Pseudo_package of component_type_id

module Real_or_pseudo_package = struct type t = real_or_pseudo_package let compare = compare end
module Real_or_pseudo_package_set = Set.Make(Real_or_pseudo_package)
module Real_or_pseudo_package_map = Map.Make(Real_or_pseudo_package)

module Real_or_pseudo_package_catalog = Data_common.Catalog(Fresh_integer)(Int_set)(Real_or_pseudo_package_set)(Int_map)(Real_or_pseudo_package_map)

let conflicts_of_repository (universe : universe) (repository_id : repository_id) =

  let repository = universe#get_repository repository_id in
  
  let package_list : (real_or_pseudo_package * package) list = 

    (* 1. Prepare the packages coming from the repository. *)
    let repository_packages_list : (package_id * package) list = 
      Package_id_set.map_to_list (fun package_id ->
        (package_id, universe#get_package package_id)
      ) repository#package_ids in

    (* 2. Prepare the pseudo packages encoding 
          the implementation of component types. *)
    let implementation_pseudo_packages_list : (component_type_id * package) list =
      Component_type_id_set.filter_map_to_list (fun component_type_id ->
        (* Filter the packages implementing the component type 
           and keep only these from the current repository. *)
        let package_ids = Package_id_set.inter 
          (universe#get_implementation component_type_id)
          repository#package_ids in
        (* Check if the component type is installable using the current repository,
           i.e. if there is at least one package which implements it. *)
        if Package_id_set.is_empty package_ids
        (* If the component type is not installable - skip it. *)
        then None
        (* If it is installable, then prepare a pseudo package, which is installable
           if-and-only-if the component type is implemented correctly. *)
        else 
          let pseudo_package = 
            (* The pseudo package simply depends on a disjunction of the packages
               implementing its corresponding component type. *)
            let depend = Package_id_set_set.singleton package_ids in
            new package ~depend () in
          Some (component_type_id, pseudo_package)
      ) universe#get_implementation_domain in

    (List.map (fun (package_id, package) -> 
      (Real_package package_id, package)
    ) repository_packages_list)
    @
    (List.map (fun (component_type_id, package) ->
      (Pseudo_package component_type_id, package)
    ) implementation_pseudo_packages_list)
  
  in

  (* 3. Prepare a catalog of string <-> package_id / component_type_id. *)
  let catalog = new Real_or_pseudo_package_catalog.obj_catalog in
  
  List.iter (fun (real_or_pseudo_package, _) ->
    catalog#add real_or_pseudo_package
  ) package_list;

  let cudf_package_name_of_real_or_pseudo_package real_or_pseudo_package =
    let id = catalog#id_of_obj real_or_pseudo_package in
    string_of_int id in

  let real_or_pseudo_package_of_cudf_package_name cudf_package_name =
    let id = int_of_string cudf_package_name in
    catalog#obj_of_id id in

  (* 4. Convert the repository and the pseudo-packages to the CUDF form. *)
  let cudf_string =
    let package_strings =
      List.map (fun (real_or_pseudo_package, package) -> 
        Cudf_of.package 
          (fun package_id -> cudf_package_name_of_real_or_pseudo_package (Real_package package_id))
          (cudf_package_name_of_real_or_pseudo_package real_or_pseudo_package)
          package
      ) package_list in
    Printf.sprintf "%s\n" (String.concat "\n\n" package_strings) in

  (* 5. Print the packages in CUDF form to a temporary file. *)
  Repository_id_set.iter (fun repository_id ->
    let repository_name = Name_of.repository_id repository_id in
    let filepath = Printf.sprintf "tmp/%s.cudf" repository_name in
    Output_helper.print_output filepath cudf_string
  ) universe#get_repository_ids

  (* 6. Use coinst to generate the conflicts file. *)
  (* 7. Parse the generated conflicts file to a conflicts structure. *)
  (* 8. Use the catalog to dereference the package_ids / component_type_ids (in fact filtering out the packages) in the "classes" structure. *)
  (* 9. Substitute the classes in conflict groups by component_type_ids, computing the euclidean multiplication where needed. *)
  (* 10. Make a set of the conflict groups list. *)



let configuration (u : universe) (c : configuration) = 
  Yojson.Safe.prettify (Json_binpacking_j.string_of_binpacking_problem (convert_configuration u c))