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
    - datatypes/Abstract_io
    - atd/Json_v0
*)

module Json   = Json_v0
module Json_t = Json.T
module Json_j = Json.J

let universe (data_model_universe : Data_model.universe) : string =
  let abstract_io_universe = Abstract_io_of.universe data_model_universe in
  let json_universe        = Json.Of_abstract_io.universe abstract_io_universe in
  Yojson.Safe.prettify (Json_j.string_of_universe json_universe)

let configuration (data_model_universe : Data_model.universe) (data_model_configuration : Data_model.configuration) : string = 
  let abstract_io_universe      = Abstract_io_of.universe      data_model_universe in
  let abstract_io_configuration = Abstract_io_of.configuration data_model_universe data_model_configuration in
  let json_configuration        = Json.Of_abstract_io.configuration abstract_io_configuration in
  Yojson.Safe.prettify (Json_j.string_of_configuration json_configuration)