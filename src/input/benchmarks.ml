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

class virtual benchmark = object (self)
  method virtual universe              : Abstract_io.universe
  method virtual initial_configuration : Abstract_io.configuration
  method virtual specification         : Abstract_io.specification
  method virtual optimization_function : Abstract_io.optimization_function

  method initial_model () : initial_model = {
    universe              = Some self#universe;
    initial_configuration = Some self#initial_configuration;
    specification         = Some self#specification;
    optimization_function = Some self#optimization_function;
  }

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

module Amazon_machine_park =
struct

  type machine_choice =
    | New_medium
    | New_large
    | New_xlarge
    | New_2xlarge
    | Old_small
    | Old_medium
    | Old_large
    | Old_xlarge
    | Micro

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
        | New_medium  -> "new-medium"
        | New_large   -> "new-large"
        | New_xlarge  -> "new-xlarge"
        | New_2xlarge -> "new-2xlarge"
        | Old_small   -> "old-small"
        | Old_medium  -> "old-medium"
        | Old_large   -> "old-large"
        | Old_xlarge  -> "old-xlarge"
        | Micro       -> "micro" in
      Printf.sprintf "%s-%d" prefix number in

    let ram = (* in MB, converted from GiB *)
      match machine_choice with
      | New_medium  ->  4026
      | New_large   ->  8052
      | New_xlarge  -> 16104
      | New_2xlarge -> 32208
      | Old_small   ->  1825
      | Old_medium  ->  4026
      | Old_large   ->  8052
      | Old_xlarge  -> 16104
      | Micro       ->   660
      in

    let vcpu = 
      match machine_choice with
      | New_medium  -> 1
      | New_large   -> 2
      | New_xlarge  -> 4
      | New_2xlarge -> 8
      | Old_small   -> 1
      | Old_medium  -> 1
      | Old_large   -> 2
      | Old_xlarge  -> 4
      | Micro       -> 1
      in

    let ecu_int = (* 2 x the real ECU *)
      match machine_choice with
      | New_medium  ->  6 (* really:                3   *)
      | New_large   -> 13 (* really:                6.5 *)
      | New_xlarge  -> 26 (* really:               13   *)
      | New_2xlarge -> 52 (* really:               26   *)
      | Old_small   ->  2 (* really:                1   *)
      | Old_medium  ->  4 (* really:                2   *)
      | Old_large   ->  8 (* really:                4   *)
      | Old_xlarge  -> 16 (* really:                8   *)
      | Micro       ->  1 (* really: unknown, up to 2   *)
      in

    let cost = (* in 1/1000 of a dollar per hour *)
      match machine_choice with
      | New_medium  -> 124 
      | New_large   -> 248 
      | New_xlarge  -> 496 (* really: 495 *)
      | New_2xlarge -> 992 (* really: 990 *)
      | Old_small   ->  65 
      | Old_medium  -> 130
      | Old_large   -> 260
      | Old_xlarge  -> 520
      | Micro       ->  20
    in

    new_location 
      ~packages_installed: []
      ~resources_provided: [("ram", ram)] (*; ("vcpu", vcpu); ("ecu", ecu_int)]*)
      ~repository:         "repository"
      ~cost:               cost
      ~name:               name

  let rec make_machines (make_machine : int -> location) n : location list =
    if n > 0 
    then (make_machine n) :: (make_machines make_machine (n - 1))
    else []

  type machine_park_choice =
    | Machine_park_old
    | Machine_park_new
    | Machine_park_old_and_new
    | Machine_park_single_type of machine_choice

  let initial_configuration_of_machine_park machine_park_choice machine_park_size : location list =
    match machine_park_choice with
    | Machine_park_old -> 
        (make_machines (new_machine Old_small  ) (machine_park_size / 4)) @ 
        (make_machines (new_machine Old_medium ) (machine_park_size / 4)) @ 
        (make_machines (new_machine Old_large  ) (machine_park_size / 4)) @
        (make_machines (new_machine Old_xlarge ) (machine_park_size / 4))

    | Machine_park_new -> 
        (make_machines (new_machine New_medium ) (machine_park_size / 4)) @ 
        (make_machines (new_machine New_large  ) (machine_park_size / 4)) @ 
        (make_machines (new_machine New_xlarge ) (machine_park_size / 4)) @
        (make_machines (new_machine New_2xlarge) (machine_park_size / 4))

    | Machine_park_old_and_new -> 
        (make_machines (new_machine Old_small  ) (machine_park_size / 8)) @ 
        (make_machines (new_machine Old_medium ) (machine_park_size / 8)) @ 
        (make_machines (new_machine Old_large  ) (machine_park_size / 8)) @
        (make_machines (new_machine Old_xlarge ) (machine_park_size / 8)) @
        (make_machines (new_machine New_medium ) (machine_park_size / 8)) @ 
        (make_machines (new_machine New_large  ) (machine_park_size / 8)) @ 
        (make_machines (new_machine New_xlarge ) (machine_park_size / 8)) @
        (make_machines (new_machine New_2xlarge) (machine_park_size / 8))

    | Machine_park_single_type (machine_choice) ->
        (make_machines (new_machine machine_choice) (machine_park_size))

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
      ];
      universe_port_hierarchy = []
    };

    method initial_configuration = {
      configuration_locations  = initial_configuration_of_machine_park machine_park_choice;
      configuration_components = [];
      configuration_bindings   = []
    }

    method specification = 
      let spec = "#Master > 0" in
      Specification_parser.main Specification_lexer.token (Lexing.from_string spec)

    method optimization_function = Abstract_io.Optimization_function_compact

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

  let dns_require_multiplier x = (2 * x) + 1 in

  object
    inherit benchmark

    method universe = 
      let universe_component_types = [
        {
          component_type_name     = "DNS-Load-Balancer";
          component_type_provide  = [("@wordpress-frontend", InfiniteProvide); ("@dns", InfiniteProvide)];
          component_type_require  = [("@wordpress-backend", dns_require_multiplier wordpress_require)];
          component_type_conflict = ["@dns"];
          component_type_consume  = []
        };
        {
          component_type_name     = "HTTP-Load-Balancer";
          component_type_provide  = [("@wordpress-frontend", InfiniteProvide)];
          component_type_require  = [("@wordpress-backend", wordpress_require)];
          component_type_conflict = [];
          component_type_consume  = []
        };
        {
          component_type_name     = "Wordpress";
          component_type_provide  = [("@wordpress-backend", (FiniteProvide 1))];
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
        ("DNS-Load-Balancer",  [ ("repository", "common_package") ]);
        ("HTTP-Load-Balancer", [ ("repository", "common_package") ]);
        ("Wordpress",          [ ("repository", "common_package") ]);
        ("MySQL",              [ ("repository", "common_package") ])
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
        universe_port_hierarchy = []
      };

    method initial_configuration = {
      configuration_locations  = initial_configuration_of_machine_park machine_park_choice;
      configuration_components = [];
      configuration_bindings   = []
    }

    method specification = 
      let spec = "#@wordpress-frontend > 0" in
      Specification_parser.main Specification_lexer.token (Lexing.from_string spec)

    method optimization_function = Abstract_io.Optimization_function_simple

  end
  
end


module Wordpress_distributed = 
struct

  open Amazon_machine_park

  class create (machine_park_choice : (machine_park_choice * int)) (wordpress_require : int) (mysql_require : int) (mysql_provide : int) (dns_consume : int) (wordpress_consume : int) (mysql_consume : int) = 

  let dns_require_multiplier  x = (2 * x) + 1 in
  let http_consume_multiplier x =  8 * x in

  object
    inherit benchmark

    method universe = 
      let universe_component_types = [
        {
          component_type_name     = "DNS-Load-Balancer";
          component_type_provide  = [("@wordpress-frontend", InfiniteProvide); ("@dns", InfiniteProvide)];
          component_type_require  = [("@wordpress-backend", dns_require_multiplier wordpress_require)];
          component_type_conflict = ["@dns"];
          component_type_consume  = [("ram", dns_consume)]
        };
        {
          component_type_name     = "HTTP-Load-Balancer";
          component_type_provide  = [("@wordpress-frontend", InfiniteProvide)];
          component_type_require  = [("@wordpress-backend", wordpress_require)];
          component_type_conflict = [];
          component_type_consume  = [("ram", http_consume_multiplier dns_consume)]
        };
        {
          component_type_name     = "Wordpress";
          component_type_provide  = [("@wordpress-backend", (FiniteProvide 1))];
          component_type_require  = [("@mysql", mysql_require)];
          component_type_conflict = [];
          component_type_consume  = [("ram", wordpress_consume)]
        };
        {
          component_type_name     = "MySQL";
          component_type_provide  = [("@mysql", (FiniteProvide mysql_provide))];
          component_type_require  = [];
          component_type_conflict = [];
          component_type_consume  = [("ram", mysql_consume)]
        }
      ]
      in
      let universe_implementation = [
        ("DNS-Load-Balancer",  [ ("repository", "common_package") ]);
        ("HTTP-Load-Balancer", [ ("repository", "common_package") ]);
        ("Wordpress",          [ ("repository", "common_package") ]);
        ("MySQL",              [ ("repository", "common_package") ])
      ] in
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
        universe_port_hierarchy = []
      };

    method initial_configuration = {
      configuration_locations  = initial_configuration_of_machine_park (fst machine_park_choice) (snd machine_park_choice);
      configuration_components = [];
      configuration_bindings   = []
    }

    method specification = 
      let spec = "#@wordpress-frontend > 0 and everywhere(#Wordpress <= 1) and everywhere(#MySQL <= 1)" in
      Specification_parser.main Specification_lexer.token (Lexing.from_string spec)

    method optimization_function = Abstract_io.Optimization_function_compact

  end
  
end

(** [create_benchmark_of_benchmark_settings benchmark_settings] creates an object of class [benchmark] following the parameters provided by [benchmark_settings]. *)
let create_benchmark_of_benchmark_settings (benchmark_settings : Settings.benchmark) : benchmark option =
  let benchmark_choice, benchmark_options = benchmark_settings in

  (* [option_fail option_name message] raises an exception about a problem [message] with benchmark's option [option_name]. *)
  let option_fail option_name option_value message = 
    let benchmark_name = List.assoc benchmark_choice Settings.benchmark_choice_assoc_revert in
    failwith (Printf.sprintf "Benchmark %s option %s problem: '%s' %s!" benchmark_name option_name option_value message) in

  (* [get_option key default] returns the value of the benchmark option [key] or the [default] value if this option was not defined. *)
  let get_option key (default : string) : string = 
    try List.assoc key benchmark_options with Not_found -> default in

  (* [get_int_option key default] tries to return the value of the benchmark option [key] (or the [default] value if this option was not defined) converted to an integer. *)
  let get_int_option key (default : int) : int = 
    let option_value = get_option key (string_of_int default) in 
    try int_of_string option_value with _ -> option_fail key option_value "is not an integer" in
  match benchmark_choice with
  
  (* Not a benchmark. *)
  | Settings.Benchmark_none -> None

  (* "Master-Slave" benchmark. *)
  | Settings.Benchmark_master_slave -> 
      let master_require = get_int_option "master_require" 10 in
      Some (new Master_worker.create master_require Simple_machine_park.Machine_park_100s Master_worker.One_worker_type)

  (* "Wordpress" benchmark. *)
  | Settings.Benchmark_wordpress -> 
      let wordpress_require = get_int_option "wordpress_require" 3 in
      let mysql_require     = get_int_option "mysql_require"     3 in 
      let mysql_provide     = get_int_option "mysql_provide"     3 in 
      let webservers        = get_int_option "webservers"        0 in 
      Some (new Wordpress.create Simple_machine_park.Machine_park_single_stub wordpress_require mysql_require mysql_provide webservers)

  (* "Distributed Wordpress" benchmark. *)
  | Settings.Benchmark_wordpress_distributed -> 
      let wordpress_require = get_int_option "wordpress_require"    3 in
      let mysql_require     = get_int_option "mysql_require"        3 in 
      let mysql_provide     = get_int_option "mysql_provide"        3 in 
      let dns_consume       = get_int_option "dns_consume"         64 in 
      let wordpress_consume = get_int_option "wordpress_consume"  512 in 
      let mysql_consume     = get_int_option "mysql_consume"      512 in 
      let machine_park_size = get_int_option "park_size"           40 in 

      let machine_park_type = 
        match (get_option "park_type" "old") with
        | "old"     -> Amazon_machine_park.Machine_park_old
        | "new"     -> Amazon_machine_park.Machine_park_new
        | "uniform" -> Amazon_machine_park.Machine_park_single_type (Amazon_machine_park.Old_small)
        | x         -> option_fail "park_type" x "is not an available type of a machine park" in

      Some (new Wordpress_distributed.create 
              (machine_park_type, machine_park_size)
              wordpress_require mysql_require mysql_provide
              dns_consume wordpress_consume mysql_consume)
