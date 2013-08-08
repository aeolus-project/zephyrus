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
    - List (standard library)
    - datatypes/Data_model (all universe, configuration, specification)
    - datatypes/Data_state (to set data)
    - input/Settings (to get files and optimization function)
    - atd/json.atd | _build/atd/json_t (json parsing: type  module)
    - atd/json.atd | _build/atd/json_j (json parsing: of_string  module)
    - input/Input_helper (for input file manipulation)
*)

val model_of_file_options :
  string option -> (* universe *)
  (string * string) list option -> (* external repositories *)
  string option -> (* configuration *)
  string option -> (* specification *)
  Settings.optim option -> (* optimization function *)
  ( Data_model_catalog.closed_model_catalog * (Data_model.resources) * (Data_model.universe option) * (Data_model.configuration option) *
    (Data_model.specification option) * (Data_model.optimization_function option)  )

val model_of_settings : unit ->
  ( Data_model_catalog.closed_model_catalog * (Data_model.resources) * (Data_model.universe option) * (Data_model.configuration option) *
    (Data_model.specification option) * (Data_model.optimization_function option)  )

val set_initial_model_of_settings : unit -> unit


(* val load_model : unit -> unit (** load all data from input files and store everything in [Data_state]  *)*)


(*
(* 1. Universe of string *)

val component_type_name_of_string    : string -> component_type_name
val port_name_of_string              : string -> port_name
val component_name_of_string         : string -> component_name
val package_name_of_string           : string -> package_name
val repository_name_of_string        : string -> repository_name
val location_name_of_string          : string -> location_name
val resource_name_of_string          : string -> resource_name
val provide_arity_of_string          : string -> provide_arity
val require_arity_of_string          : string -> require_arity
val resource_consumption_of_string   : string -> resource_consumption
val resource_provide_arity_of_string : string -> resource_provide_arity
val component_type_of_string         : string -> component_type
val component_types_of_string        : string -> component_type list
val package_of_string                : string -> package
val packages_of_string               : string -> package list
val repository_of_string             : string -> repository
val repositories_of_string           : string -> repository list
val package_names_of_string          : string -> package_name list
val universe_of_string               : string -> universe


(* 2. Configuration of string *)

val location_of_string      : string -> location
val configuration_of_string : string -> configuration


(* 3. Specification of string *)

val specification_of_string      : string -> specification
val specification_of_string_json : string -> specification


(* 4. Main load functions *)

val load_universe              : unit -> unit (* TODO: extend the package names *)
val load_universe_flat         : unit -> unit (* TODO: not implemented yet *)
val load_configuration         : unit -> unit
val load_specification         : unit -> unit
val load_optimization_function : unit -> unit

val load_model : unit -> unit
*)

