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
    - datatypes/Data_common
    - datatypes/Data_model
*)

open Data_common
open Data_model

(* TODO: Very temporary simplified ad-hoc file handling and external program execution in this module! *)

type real_or_pseudo_package =
  | Real_package   of package_id
  | Pseudo_package of component_type_id

module Real_or_pseudo_package = struct type t = real_or_pseudo_package let compare = compare end
module Real_or_pseudo_package_set = Set.Make(Real_or_pseudo_package)
module Real_or_pseudo_package_map = Map.Make(Real_or_pseudo_package)

module Real_or_pseudo_package_catalog = 
  Data_common.Catalog(Fresh_integer)(Int_set)(Real_or_pseudo_package_set)(Int_map)(Real_or_pseudo_package_map)

let repository universe repository_id =

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
  let repository_name = Name_of.repository_id repository_id in
  let cudf_filepath = Printf.sprintf "tmp/%s.cudf" repository_name in
  Output_helper.print_output cudf_filepath cudf_string;

  (* 6. Use coinst to generate the conflicts file. *)
  let conflicts_filepath = Printf.sprintf "tmp/%s-conflicts.json" repository_name in
  if not (Engine_helper.program_is_available Engine_helper.coinst) then 
    Zephyrus_log.log_panic "The coinst external tool cannot be found. Aborting execution\n";
    let coinst_exit_code = 
      Unix.system (Printf.sprintf "coinst -all -conflicts %s -cudf %s" conflicts_filepath cudf_filepath) in
    if not (Engine_helper.did_program_exit_ok coinst_exit_code) then
      Zephyrus_log.log_panic "The coinst external tool exited abnormally. Aborting execution\n";

  (* 7. Parse the generated conflicts file to a conflicts structure. *)
  match Input_helper.parse_json Coinst_conflicts_j.read_coinst_conflicts conflicts_filepath with
  | None -> Component_type_id_set_set.empty
  | Some coinst_conflicts ->

  (* 8. Use the catalog to dereference the package_ids / component_type_ids (in the same time filtering out the real packages) in the "classes" structure. *)
  let classes : (string * (component_type_id list)) list =
    List.map (fun (class_name, class_members) ->
      let class_members : component_type_id list =
        List.filter_map (fun cudf_package_name -> 
          match real_or_pseudo_package_of_cudf_package_name cudf_package_name with
          | Real_package _ -> None
          | Pseudo_package component_type_id -> Some component_type_id
        ) class_members in
      (class_name, class_members)
    ) coinst_conflicts.Coinst_conflicts_t.classes in

  (* 9. Substitute the classes in conflict groups by component_type_ids, computing the cartesian product where needed. *)
  let incompatibilities : component_type_id list list = 
    List.flatten (
      List.map (fun (conflict_group : Coinst_conflicts_t.incompatibility) ->
        List.cartesian_product (
          List.map (fun class_name ->
            List.assoc class_name classes
          ) conflict_group)
      ) coinst_conflicts.Coinst_conflicts_t.incompatibilities) in

  (* 10. Make the list of lists a set of sets. *)
  let incompatibilities_set : Component_type_id_set_set.t =
    Component_type_id_set_set.of_list Component_type_id_set.of_list_directly incompatibilities in
  
  (* 11. Return the result. *)
  incompatibilities_set


let universe universe =
  let incompatibilities_list : (repository_id * Component_type_id_set_set.t) list =
    Repository_id_set.map_to_list (fun repository_id ->
      let incompatibilities = repository universe repository_id in
      (repository_id, incompatibilities)
    ) universe#get_repository_ids in
  Repository_id_map.of_assoc_list incompatibilities_list
