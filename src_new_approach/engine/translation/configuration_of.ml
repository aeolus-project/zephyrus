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

open Data_model
open Data_constraint

let solution (universe : universe) (initial_configuration : configuration) (solution : solution) : configuration =

  (* Locations *)
  let location_ids = initial_configuration#get_location_ids in

  let location_id_to_location_map : (location Location_id_map.t) ref = ref Location_id_map.empty in
  let get_location id = Location_id_map.find id !location_id_to_location_map in
(*
  let location_id_to_location_name_map : (location_name Location_id_map.t) ref = ref Location_id_map.empty in
  let location_name_to_location_id_map = 
*)
  object(self)

    method get_location l = failwith "unimplemented" (* Location_id_map.find l (!location_get) *)
    method get_component c = failwith "unimplemented" (* Component_id_map.find c (!component_get) *)

    method get_locations  = failwith "unimplemented" (* !locations *)
    method get_components = failwith "unimplemented" (* !components *)
    method get_bindings   = failwith "unimplemented" (* implem_bindings *)

    method get_location_ids  = failwith "unimplemented" (* !location_ids *)
    method get_component_ids = failwith "unimplemented" (* !component_ids *)

    method get_location_names  = failwith "unimplemented" (* !location_names *)
    method get_component_names = failwith "unimplemented" (* !component_names *)

    method c_l = failwith "unimplemented" (* self#get_location_ids *)
    method c_c = failwith "unimplemented" (* self#get_component_ids *)
    method c_type c = failwith "unimplemented" (* (self#get_component c)#typ *)

    method get_local_component l t = failwith "unimplemented" (* (try Location_component_type_map.find (l,t) implem_get_local_component with
           | Not_found -> let tmp = get_local_component l t (!component_get) in
             implem_get_local_component <- Location_component_type_map.add (l,t) tmp implem_get_local_component; tmp) *)
    method get_local_package l k = failwith "unimplemented" (* (try Location_package_map.find (l,k) implem_get_local_package with
           | Not_found -> let tmp = get_local_package l k (!location_get) in
             implem_get_local_package <- Location_package_map.add (l,k) tmp implem_get_local_package; tmp); *)

    method get_location_id l  = failwith "unimplemented" (* Location_name_map.find l (!location_get_id) *)
    method get_component_id c = failwith "unimplemented" (* Component_name_map.find c (!component_get_id) *)

    method get_location_name l = failwith "unimplemented" (* Location_id_map.find l (!location_get_name) *)
    method get_component_name c = failwith "unimplemented" (* Component_id_map.find c (!component_get_name) *)
  end