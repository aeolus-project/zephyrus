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

open Aeolus_types_t
open Typing_context
open Variables


type variable_kind =
  | BooleanVariable
  | NaturalVariable

let variable_kind variable =
  match variable with
  | GlobalElementVariable   _ -> NaturalVariable
  | LocalElementVariable    _ -> NaturalVariable
  | BindingVariable         _ -> NaturalVariable
  | LocalRepositoryVariable _ -> BooleanVariable
  | LocalResourceVariable   _ -> NaturalVariable
  | SpecificationVariable   _ -> NaturalVariable




let get_all_global_element_variables universe =
  List.map (fun element ->
    (GlobalElementVariable element)
  ) (get_elements universe)

let get_all_local_element_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun element ->
        (LocalElementVariable (location_name, element))
      ) (get_elements universe)
    ) (get_location_names configuration)
  )

let get_all_binding_variables universe =
  List.flatten ( List.flatten (
    List.map (fun port_name ->
      List.map (fun providing_component_type_name ->
        List.map (fun requiring_component_type_name ->
          (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
        ) (requirers universe port_name)
      ) (providers universe port_name)
    ) (get_port_names universe)
  ))

let get_all_local_repository_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun repository_name ->
        (LocalRepositoryVariable (location_name, repository_name))
      ) (get_repository_names universe)
    ) (get_location_names configuration)
  )

let get_all_local_resource_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun resource_name ->
        (LocalResourceVariable (location_name, resource_name))
      ) (get_resource_names universe)
    ) (get_location_names configuration)
  )

let pred_specification_variable variable =
  match variable with
  | SpecificationVariable _   -> true
  | _                         -> false

let get_all_specification_variables specification configuration =
  let all_variables_from_specification =
    Specification_constraints.extract_variables_from_specification configuration specification
  in
  let specification_variables =
    List.filter pred_specification_variable all_variables_from_specification
  in
  specification_variables


let get_all_variables universe configuration specification = 
  List.flatten
    [get_all_global_element_variables   universe                   ;
     get_all_local_element_variables    universe      configuration;
     get_all_binding_variables          universe                   ;
     get_all_local_repository_variables universe      configuration;
     get_all_local_resource_variables   universe      configuration;
     get_all_specification_variables    specification configuration]