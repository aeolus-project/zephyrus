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
    - Data_common.
    - Json_versions_t
    - Stateful_json_v1_j
*)

open Data_common
open Abstract_io

let read_versioned_object_from_file (file : string) : Json_versions_t.versioned_object option =
  Input_helper.parse_json Json_versions_j.read_versioned_object file

let read_version_from_file (file : string) : int =
  let versioned_object = read_versioned_object_from_file file in
  match versioned_object with
  | Some versioned_object' -> versioned_object'.Json_versions_t.version
  | None -> 0

let read_universe_from_file (file : string) : universe option =
  let syntax_version = read_version_from_file file in
  Zephyrus_log.log_execution (Printf.sprintf "Universe syntax version = %d%!\n" syntax_version);
  match syntax_version with
  | 1 -> (match Input_helper.parse_json Stateful_json_v1_j.read_universe file with
          | None          -> None
          | Some universe -> Some (Stateful_json_v1.To_abstract_io.universe universe))
  | _ -> failwith "Unsupported universe syntax version!"

let read_repository_from_file (repository_name : repository_name) (repository_file : string) : repository option = 
  let repository_packages = Input_helper.parse_json Stateful_json_v1_j.read_packages repository_file in 
  match repository_packages with
  | None                     -> None
  | Some repository_packages -> Some 
    ({ 
      repository_name     = repository_name;
      repository_packages = List.map Stateful_json_v1.To_abstract_io.package repository_packages
    })

let read_repositories_from_files (repository_names_and_files : (string * string) list) : repository list = 
  List.filter_map (fun (repository_name, repository_file) -> 
    read_repository_from_file repository_name repository_file
  ) repository_names_and_files

let read_configuration_from_file (file : string) : configuration option = 
  let syntax_version = read_version_from_file file in
  Zephyrus_log.log_execution (Printf.sprintf "Configuration syntax version = %d%!\n" syntax_version);
    match syntax_version with
  | 1 -> (match Input_helper.parse_json Stateful_json_v1_j.read_configuration file with
          | None   -> None
          | Some c -> Some (Stateful_json_v1.To_abstract_io.configuration c))
  | _ -> failwith "Unsupported configuration syntax version!"


let read_specification_from_file (file : string) : specification option = 
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token file

let optimization_function_of_settings_optimization_function (optimization_function : Settings.optimization_function) : optimization_function =
  match optimization_function with
  | Settings.Optimization_function_none         -> Optimization_function_none
  | Settings.Optimization_function_simple       -> Optimization_function_simple
  | Settings.Optimization_function_compact      -> Optimization_function_compact
  | Settings.Optimization_function_conservative -> Optimization_function_conservative
  | Settings.Optimization_function_spread       -> Optimization_function_spread

let read_from_file_options ~universe_file ~repository_files ~configuration_file ~specification_file ~optimization_function_choice : initial_model =

  (* Read model data from files into the form. *)
  let universe : universe          option = 
    let universe                              : universe   option = Option.map_flatten read_universe_from_file universe_file in
    let additional_repositories               : repository list   = List.of_option (Option.map read_repositories_from_files repository_files) in
    let universe_with_additional_repositories : universe   option = Option.map (fun universe -> universe_add_repositories universe additional_repositories) universe in
    universe_with_additional_repositories in
    
  let configuration         : configuration         option = Option.map_flatten read_configuration_from_file                            configuration_file in
  let specification         : specification         option = Option.map_flatten read_specification_from_file                            specification_file in
  let optimization_function : optimization_function option = Option.map         optimization_function_of_settings_optimization_function optimization_function_choice in

  {
    universe              = universe;
    initial_configuration = configuration;
    specification         = specification;
    optimization_function = optimization_function;
  }

let from_settings () = 
  read_from_file_options
    (Settings.get_input_file_universe              ())
    (Settings.get_input_file_repositories          ())
    (Settings.get_input_file_initial_configuration ())
    (Settings.get_input_file_specification         ())
    (Settings.get_input_optimization_function      ())




