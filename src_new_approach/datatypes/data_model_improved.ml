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
    - datatypes/Functions_model
*)

open Functions_model



module Component_type_names_of_component_types = Data_common.Set.Convert(Data_model.Component_type_set)(Data_model.Component_type_name_set)
let component_type_names_of_component_types set = Component_type_names_of_component_types.convert (fun c -> c.Data_model.component_type_name) set

class universe u = object(res)
  (* private *)
  val mutable implem_u_dt = None;
  val mutable implem_u_dp = None;
  val mutable implem_u_dr = None;
  val mutable implem_u_dk = None;
    
  val mutable implem_u_i = Data_model.Component_type_name_map.empty;
  val mutable implem_u_w = Data_model.Package_name_map.empty;

  val mutable implem_ur = Data_model.Port_name_map.empty;
  val mutable implem_up = Data_model.Port_name_map.empty;
  val mutable implem_uc = Data_model.Port_name_map.empty;

  val mutable implem_get_component_types = None;
  val mutable implem_get_repositories    = None;

  (* methods *)
  method universe_component_types = u.Data_model.universe_component_types;
  method universe_implementation  = u.Data_model.universe_implementation;
  method universe_repositories    = u.Data_model.universe_repositories;

  method get_component_types = (fun () -> match implem_get_component_types with | Some(set) -> set
           | None -> let tmp = Core.get_component_types res#universe_component_types in implem_get_component_types <- Some(tmp); tmp);
  method get_repositories    = (fun () -> match implem_get_repositories with | Some(set) -> set
           | None -> let tmp = Core.get_repositories res#universe_repositories in implem_get_repositories <- Some(tmp); tmp);

  method u_dt = (fun () -> match implem_u_dt with | Some(set) -> set
           | None -> let tmp = Core.get_component_type_names res#universe_component_types in implem_u_dt <- Some(tmp); tmp);
  method u_dp = (fun () -> match implem_u_dp with | Some(set) -> set
           | None -> let tmp = Core.get_port_names (res#get_component_types ()) in implem_u_dp <- Some(tmp); tmp);
  method u_dr = (fun () -> match implem_u_dr with | Some(set) -> set
           | None -> let tmp = Core.get_repository_names res#universe_repositories in implem_u_dr <- Some(tmp); tmp);
  method u_dk = (fun () -> match implem_u_dk with | Some(set) -> set
           | None -> let tmp = Core.get_package_names (res#get_repositories ()) in implem_u_dk <- Some(tmp); tmp);

  method u_i = (fun t -> try Data_model.Component_type_name_map.find t implem_u_i with
           | Not_found -> let tmp = Core.get_component_type_implementation res#universe_implementation t in 
             implem_u_i <- Data_model.Component_type_name_map.add t tmp implem_u_i; tmp);
  method u_w = (fun k -> (if implem_u_w = Data_model.Package_name_map.empty then (
     let add_repository r =
       let merge_function n k1 k2 = match (k1,k2) with
         | (Some(k1'), Some(k2')) -> Some(k1')
         | (Some(k1'), None     ) -> Some(k1')
         | (None     , Some(k2')) -> Some(k2')
         | (None     , None     ) -> None in implem_u_w <- Data_model.Package_name_map.merge merge_function implem_u_w r.Data_model.repository_packages in
      Data_model.Repository_set.iter add_repository (res#get_repositories ())));
          try Data_model.Package_name_map.find k implem_u_w with
           | Not_found -> Zephyrus_log.log_missing_data "package" k "universe"; exit(-1));
   method ur = (fun p -> try Data_model.Port_name_map.find p implem_ur with
           | Not_found -> let tmp = component_type_names_of_component_types (Core.requirers (res#get_component_types ()) p) in
             implem_ur <- Data_model.Port_name_map.add p tmp implem_ur; tmp);
   method up = (fun p -> try Data_model.Port_name_map.find p implem_up with
           | Not_found -> let tmp = component_type_names_of_component_types (Core.providers (res#get_component_types ()) p) in
             implem_up <- Data_model.Port_name_map.add p tmp implem_up; tmp);

  method uc = (fun p -> try Data_model.Port_name_map.find p implem_uc with
           | Not_found -> let tmp = component_type_names_of_component_types (Core.conflicters (res#get_component_types ()) p) in
             implem_uc <- Data_model.Port_name_map.add p tmp implem_uc; tmp);

  method get_component_type = (fun n -> Core.get_component_type res#universe_component_types n);
  method get_repository = (fun n -> Core.get_repository res#universe_repositories n);
  method get_package = res#u_w;
end



module Location_component_type = struct
  type t = Data_model.location_name * Data_model.component_type_name
  let compare (l,t) (l',t') = let tmp = Data_model.Location_name.compare l l' in
    if tmp = 0 then Data_model.Component_type_name.compare t t' else tmp
end module Location_component_type_map = Data_common.Map.Make(Location_component_type)

module Location_package = struct
  type t = Data_model.location_name * Data_model.package_name
  let compare (l,t) (l',t') = let tmp = Data_model.Location_name.compare l l' in
    if tmp = 0 then Data_model.Package_name.compare t t' else tmp
end module Location_package_map = Data_common.Map.Make(Location_package)


class configuration c =  object(self)
    (*  private *)
    val mutable implem_c_l = None;
    val mutable implem_c_c = None;

    val mutable implem_get_local_component = Location_component_type_map.empty;
    val mutable implem_get_local_package = Location_package_map.empty;

    (* methods *)
    method configuration_locations  = c.Data_model.configuration_locations;
    method configuration_components = c.Data_model.configuration_components;
    method configuration_bindings   = c.Data_model.configuration_bindings;

    method c_l = (fun () -> match implem_c_l with
           | Some(set) -> set
           | None -> let tmp = Core.get_location_names (self#configuration_locations) in implem_c_l <- Some(tmp); tmp);
    method c_c = (fun () -> match implem_c_c with
           | Some(set) -> set
           | None -> let tmp = Core.get_component_names (self#configuration_locations) in implem_c_l <- Some(tmp); tmp);
    method c_type = (fun n -> try (Data_model.Component_name_map.find n self#configuration_components).Data_model.component_type with
           | Not_found -> Zephyrus_log.log_missing_data "component" n "configuration"; exit(-1));

    method get_local_component = (fun l t -> try Location_component_type_map.find (l,t) implem_get_local_component with
           | Not_found -> let tmp = Core.get_local_component l t self#configuration_components in
             implem_get_local_component <- Location_component_type_map.add (l,t) tmp implem_get_local_component; tmp);
    method get_local_package = (fun l k -> try Location_package_map.find (l,k) implem_get_local_package with
           | Not_found -> let tmp = Core.get_local_package l t self#configuration_locations in
             implem_get_local_package <- Location_package_map.add (l,k) tmp implem_get_local_package; tmp);
end





