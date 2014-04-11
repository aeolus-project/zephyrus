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

(** Measure the model. *)

open Data_model

class model ~universe ~configuration () = object (self : 'selftype)

  val universe      : universe      = universe
  val configuration : configuration = configuration
 
  (** [number_of_packages_on_location location_id package_id] returns [1] if there is a package with id [package_id] installed on the location with id [location_id] and [0] otherwise. *)
  method number_of_packages_on_location location_id package_id : int =
    if Package_id_set.mem package_id (configuration#get_location location_id)#packages_installed then 1 else 0

  (** [number_of_packages_global package_id] returns the total number of packages with id [package_id] installed on all the locations. *)
  method number_of_packages_global package_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (self#number_of_packages_on_location location_id package_id)
    ) configuration#get_location_ids 0

  (** [number_of_any_packages_global ()] returns the total number of packages installed on all the locations. *)
  method number_of_any_packages_global () : int = 
    Location_id_set.fold (fun location_id sum ->
      sum + (Package_id_set.cardinal (self#get_location_package_ids location_id))
    ) configuration#get_location_ids 0

  (** [number_of_components_on_location location_id component_type_id] returns the number of components of type with id [component_type_id] installed on the location with id [location_id]. *)
  method number_of_components_on_location location_id component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      (component#location = location_id) && (component#typ = component_type_id)
    ) configuration#get_component_ids)

  (** [number_of_components_global component_type_id] returns the total number of components of type with id [component_type_id] installed on all the locations. *)
  method number_of_components_global component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      component#typ = component_type_id
    ) configuration#get_component_ids)
  
  (** [number_of_any_components_global ()] returns the total number of components installed on all the locations. *)
  method number_of_any_components_global () : int =
    Component_id_set.cardinal configuration#get_component_ids

  (** [number_of_ports_provided_on_location location_id port_id] returns the total number of port arity of port [port_id] provided by all the components installed on the location with id [location_id]. *)
  method number_of_ports_provided_on_location location_id port_id : int =
    let int_of_provide_arity = function
      | Infinite_provide -> max_int
      | Finite_provide a' -> a'
    in
    Component_id_set.fold (fun component_id sum -> 
      let component = configuration#get_component component_id in
      let provide_arity =
        if component#location = location_id then 
          let component_type = universe#get_component_type (component#typ) in
          if Port_id_set.mem port_id component_type#provide_domain
          then int_of_provide_arity (component_type#provide port_id)
          else 0
        else 0 in
      (sum + provide_arity)
    ) configuration#get_component_ids 0

  (** [number_of_ports_provided_global port_id] returns the total number of port arity of port [port_id] provided by all the components installed on all the locations. *)
  method number_of_ports_provided_global port_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (self#number_of_ports_provided_on_location location_id port_id)
    ) configuration#get_location_ids 0

  (** [get_location_component_ids location_id] returns a set of component ids of all the components installed on the location with id [location_id]. *)
  method get_location_component_ids location_id : Component_id_set.t =
    Component_id_set.filter (fun component_id ->
      let component = configuration#get_component component_id in
      component#location = location_id
    ) configuration#get_component_ids

  (** [get_location_package_ids location_id] returns a set of package ids of all the packages installed on the location with id [location_id]. *)
  method get_location_package_ids location_id : Package_id_set.t =
    let location = configuration#get_location location_id in
    location#packages_installed

  (** [location_is_empty ?(with_packages) location_id] returns [true] if there are no components (nor packages if the argument [with_packages] is [true]) installed on the location with id [location_id], [false] otherwise. *)  
  method location_is_empty ?(with_packages = true) location_id : bool =
    (Component_id_set.is_empty (self#get_location_component_ids location_id))
    &&
    (if with_packages
     then (Package_id_set.is_empty (self#get_location_package_ids location_id))
     else true)

  (** [location_is_used ?(with_packages) location_id] returns true if there are any components (or packages if the argument [with_packages] is [true]) installed on the location with id [location_id], [false] otherwise. *)  
  method location_is_used ?(with_packages = true) location_id : bool =
    not (self#location_is_empty ~with_packages location_id)

  (** [get_used_location_ids ()] returns a set of location ids of all the locations which are used, i.e. have any components (or packages if the argument [with_packages] is [true]) installed on them. *)
  method get_used_location_ids ?(with_packages = true) () : Location_id_set.t =
    Location_id_set.filter (self#location_is_used ~with_packages) configuration#get_location_ids

  (** [used_locations_cost ?(with_packages) ()] returns the sum of costs of all the used locations, i.e. those which have any components (or packages if the argument [with_packages] is [true]) installed on them. *)
  method used_locations_cost ?(with_packages = true) () : location_cost =
    Location_id_set.fold (fun location_id sum ->
      let location = configuration#get_location location_id in
      sum + location#cost
    ) (self#get_used_location_ids ~with_packages ()) 0

end

type optimality_measure = int list

let measure_optimality ?(with_packages = true) (optimization_function : optimization_function) (universe : universe) (configuration : configuration) : optimality_measure =
  let measure_model = new model ~universe ~configuration () in
  match optimization_function with

  | Optimization_function_simple -> 
      [measure_model#number_of_any_components_global ()] (* Minimize the number of components. *)
  
  | Optimization_function_compact -> 
      [measure_model#used_locations_cost ();             (* First minimize the number of used locations, *) 
       measure_model#number_of_any_components_global (); (* then minimize the number of components, *)
       measure_model#number_of_any_packages_global ()]   (* finally minimize the number of packages (so we do not have useless packages). *)

  | Optimization_function_conservative -> []
  | Optimization_function_spread       -> []
  | Optimization_function_none         -> []
