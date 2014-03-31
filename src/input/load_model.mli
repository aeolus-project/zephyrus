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

(** Conversion of the Zephyrus abstract IO syntax to a full Zephyrus model in our internal representation.  *)

(* Depends on
    - List (standard library)
    - datatypes/Data_model (all universe, configuration, specification)
    - datatypes/Data_state (to set data)
    - input/Settings (to get files and optimization function)
    - atd/json.atd | _build/atd/json_t (json parsing: type  module)
    - atd/json.atd | _build/atd/json_j (json parsing: of_string  module)
    - input/Input_helper (for input file manipulation)
*)

type initial_model = {
  universe              : Data_model.universe              option;
  initial_configuration : Data_model.configuration         option;
  specification         : Data_model.specification         option;
  optimization_function : Data_model.optimization_function option;
}

val model_of_file_options :
  string option ->                 (* universe *)
  (string * string) list option -> (* external repositories *)
  string option ->                 (* configuration *)
  string option ->                 (* specification *)
  Settings.optim option ->         (* optimization function *)
  ( Data_model_catalog.closed_model_catalog * initial_model )

val empty_configuration : Data_model.configuration

val model_of_settings          : unit                 -> (Data_model_catalog.closed_model_catalog * initial_model)
val initial_model_of_settings  : unit                 -> (Data_model_catalog.closed_model_catalog * initial_model)
val initial_model_of_benchmark : Benchmarks.benchmark -> (Data_model_catalog.closed_model_catalog * initial_model)

