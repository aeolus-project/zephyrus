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

class universe : Data_model.universe -> object
  (* private *)
  val mutable implem_u_dt : Data_model.Component_type_name_set.t option;
  val mutable implem_u_dp : Data_model.Port_name_set.t option;
  val mutable implem_u_dr : Data_model.Repository_name_set.t option;
  val mutable implem_u_dk : Data_model.Package_name_set.t option;
    
  val mutable implem_u_i : Data_model.Package_name_set.t Data_model.Component_type_name_map.t;
  val mutable implem_u_w : Data_model.package Data_model.Package_name_map.t;

  val mutable implem_ur : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;
  val mutable implem_up : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;
  val mutable implem_uc : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;

  val mutable implem_get_component_types : Data_model.Component_type_set.t option;
  val mutable implem_get_repositories    : Data_model.Repository_set.t option;

  (* methods *)
  method universe_component_types : Data_model.component_type Data_model.Component_type_name_map.t;     (** Component types available in this universe. *)
  method universe_implementation  : Data_model.Package_name_set.t Data_model.Component_type_name_map.t; (** Which packages implement the component types of this universe. *)
  method universe_repositories    : Data_model.repository Data_model.Repository_name_map.t;             (** Package repositories available in this universe. *)

  method get_component_types : unit -> Data_model.Component_type_set.t;
  method get_repositories    : unit -> Data_model.Repository_set.t;

  method u_dt : unit -> Data_model.Component_type_name_set.t;
  method u_dp : unit -> Data_model.Port_name_set.t;
  method u_dr : unit -> Data_model.Repository_name_set.t;
  method u_dk : unit -> Data_model.Package_name_set.t;
    
  method u_i : Data_model.component_type_name -> Data_model.Package_name_set.t;
  method u_w : Data_model.package_name -> Data_model.package;

  method ur : Data_model.port_name -> Data_model.Component_type_name_set.t;
  method up : Data_model.port_name -> Data_model.Component_type_name_set.t;
  method uc : Data_model.port_name -> Data_model.Component_type_name_set.t;

  method get_component_type : Data_model.component_type_name -> Data_model.component_type;
  method get_repository     : Data_model.repository_name -> Data_model.repository;
  method get_package        : Data_model.package_name -> Data_model.package;
end


class configuration : Data_model.configuration -> object
    (*  private *)
    val mutable implem_c_l : Data_model.Location_name_set.t option;
    val mutable implem_c_c : Data_model.Component_name_set.t option;

    val mutable implem_get_local_component : Data_model.Component_name_set.t Location_component_type_map.t;
    val mutable implem_get_local_package : bool Location_package_map.t;

    (* methods *)
    method configuration_locations  : Data_model.location Data_model.Location_name_map.t;
    method configuration_components : Data_model.component Data_model.Component_name_map.t;
    method configuration_bindings   : Data_model.Binding_set.t;

    method c_l : unit -> Data_model.Location_name_set.t;
    method c_c : unit -> Data_model.Component_name_set.t;
    method c_type : Data_model.component_name -> Data_model.component_type_name;

    method get_local_component : Data_model.location_name -> Data_model.component_type_name -> Data_model.Component_name_set.t;
    method get_local_package : Data_model.location_name -> Data_model.package_name -> bool
end




