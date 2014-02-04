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

(** Create benchmark input data. *)

open Abstract_io

class virtual benchmark = object
  method virtual universe              : Abstract_io.universe
  method virtual initial_configuration : Abstract_io.configuration
  method virtual specification         : Abstract_io.specification
  method virtual optimisation_function : Data_model.optimization_function
end

module Simple_machine_park =
struct

  type machine_choice =
    | Stub
    | Small
    | Medium
    | Large

  let new_location ?(resources_provided = []) ?(packages_installed = []) ?(cost = 1) ~repository ~name : location = {
    location_name               = name;
    location_provide_resources  = resources_provided;
    location_repository         = repository;
    location_packages_installed = packages_installed;
    location_cost               = cost
  }

  let new_machine machine_choice number : location =
    let name =
      let prefix = 
        match machine_choice with
        | Stub   -> "stub-location"
        | Small  -> "small-location"
        | Medium -> "medium-location"
        | Large  -> "large-location" in
        Printf.sprintf "%s-%d" prefix number in

    let ram = 
      match machine_choice with
      | Stub   -> 0
      | Small  -> 2048
      | Medium -> 8192
      | Large  -> 32768 in

    let cost = 
      match machine_choice with
      | Stub   -> 1
      | Small  -> 1
      | Medium -> 3
      | Large  -> 11 in

    new_location 
      ~packages_installed: []
      ~resources_provided: [("ram", ram)]
      ~repository:         "repository"
      ~cost:               cost
      ~name:               name

  let rec make_machines (make_machine : int -> location) n : location list =
    if n > 0 
    then (make_machine n) :: (make_machines make_machine (n - 1))
    else []

  type machine_park_choice =
    | Machine_park_single_stub
    | Machine_park_100s
    | Machine_park_50s_50m
    | Machine_park_33s_33m_33l

  let initial_configuration_of_machine_park machine_park_choice : location list =
    match machine_park_choice with
    | Machine_park_single_stub -> (make_machines (new_machine Stub ) 1)
    | Machine_park_100s        -> (make_machines (new_machine Small) 100)
    | Machine_park_50s_50m     -> (make_machines (new_machine Small) 50) @ (make_machines (new_machine Medium) 50)
    | Machine_park_33s_33m_33l -> (make_machines (new_machine Small) 33) @ (make_machines (new_machine Medium) 33) @ (make_machines (new_machine Large) 33)

end

module Master_worker = 
struct

  open Simple_machine_park

  type complexity =
    | One_worker_type
    | Two_worker_types_with_conflicting_packages
  
  class create (master_require : int) (machine_park_choice : machine_park_choice) (complexity : complexity) = 
  object
    inherit benchmark

    method universe = {
      universe_component_types = [
        {
          component_type_name     = "Master";
          component_type_provide  = [];
          component_type_require  = [("@work", master_require)];
          component_type_conflict = [];
          component_type_consume  = [("ram", 1024)]
        };
        {
          component_type_name     = "Worker";
          component_type_provide  = [("@work", (FiniteProvide 1))];
          component_type_require  = [];
          component_type_conflict = [];
          component_type_consume  = [("ram", 1024)]
        }
      ];
      universe_implementation = [
        ("Master", [ ("repository", "common_package") ]);
        ("Worker", [ ("repository", "common_package") ])
      ];
      universe_repositories = [
        {
          repository_name = "repository";
          repository_packages = [
            {
              package_name     = "common_package";
              package_depend   = [];
              package_conflict = [];
              package_consume  = []
            }
          ]
        }
      ]
    };

    method initial_configuration = {
      configuration_locations  = initial_configuration_of_machine_park machine_park_choice;
      configuration_components = [];
      configuration_bindings   = []
    }

    method specification = 
      let spec = "#Master > 0" in
      Specification_parser.main Specification_lexer.token (Lexing.from_string spec)

    method optimisation_function = Data_model.Optimization_function_compact

  end
  
end


module Wordpress = 
struct

  open Simple_machine_park

  let make_webserver number =
    let name =
      let prefix = "Webserver" in
      Printf.sprintf "%s-%d" prefix number in
    {
      component_type_name     = name;
      component_type_provide  = [("@webserver", (FiniteProvide 1))];
      component_type_require  = [];
      component_type_conflict = [];
      component_type_consume  = [];
    }

  let rec make_webservers n : component_type list =
    if n > 0 
    then (make_webserver n) :: (make_webservers (n - 1))
    else []

  class create (machine_park_choice : machine_park_choice) (wordpress_require : int) (mysql_require : int) (mysql_provide : int) (number_of_webservers : int) = 

  let webservers = make_webservers number_of_webservers in

  object
    inherit benchmark

    method universe = 
      let universe_component_types = [
        {
          component_type_name     = "Load-Balancer";
          component_type_provide  = [];
          component_type_require  = [("@wordpress", wordpress_require)];
          component_type_conflict = [];
          component_type_consume  = []
        };
        {
          component_type_name     = "Wordpress";
          component_type_provide  = [("@wordpress", (FiniteProvide 1))];
          component_type_require  = [("@mysql", mysql_require)] @ (if number_of_webservers = 0 then [] else [("@webserver", 1)]);
          component_type_conflict = [];
          component_type_consume  = []
        };
        {
          component_type_name     = "MySQL";
          component_type_provide  = [("@mysql", (FiniteProvide mysql_provide))];
          component_type_require  = [];
          component_type_conflict = [];
          component_type_consume  = []
        }
      ] @ webservers
      in
      let universe_implementation = [
        ("Load-Balancer", [ ("repository", "common_package") ]);
        ("Wordpress",     [ ("repository", "common_package") ]);
        ("MySQL",         [ ("repository", "common_package") ])
      ] @ (List.map (fun webserver -> (webserver.component_type_name, [ ("repository", "common_package") ])) webservers) in
      let universe_repositories = [
        {
          repository_name = "repository";
          repository_packages = [
            {
              package_name     = "common_package";
              package_depend   = [];
              package_conflict = [];
              package_consume  = []
            }
          ]
        }
      ] in

      {
        universe_component_types = universe_component_types;
        universe_implementation  = universe_implementation;
        universe_repositories    = universe_repositories;
      };

    method initial_configuration = {
      configuration_locations  = initial_configuration_of_machine_park machine_park_choice;
      configuration_components = [];
      configuration_bindings   = []
    }

    method specification = 
      let spec = "#Load-Balancer > 0" in
      Specification_parser.main Specification_lexer.token (Lexing.from_string spec)

    method optimisation_function = Data_model.Optimization_function_simple

  end
  
end
