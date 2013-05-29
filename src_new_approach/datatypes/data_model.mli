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

(********************)
(** 1. Basic Types **)
(********************)

type component_type_name = string
type port_name           = string
type component_name      = string
type package_name        = string
type repository_name     = string
type location_name       = string
type resource_name       = string
type spec_variable_name  = string

type provide_arity = | Finite of int | Infinite
type require_arity = int

type binding = { b_port : port_name; b_requires : component_name; b_provides : component_name }

module Map_component_type_name : Data_common.Map.S with key = component_type_name
module Map_component_name : Data_common.Map.S with key = component_name
module Map_port_name : Data_common.Map.S with key = port_name
module Set_port_name : Data_common.Set.S with elt = port_name
module Set_binding : Data_common.Set.S with elt = binding


(********************)
(** 2. Flat Model  **)
(********************)

module Aeolus_flat : sig
  type element =
    | ComponentType of component_type_name
    | Port          of port_name
  type semantic_error = { el : element; missing : element; }

  type component_type = { provides : provide_arity Map_port_name.t; requires : require_arity Map_port_name.t; conflicts : Set_port_name.t }
  type configuration = { w : component_type_name Map_component_name.t; b : Set_binding.t }
  type universe = { n : component_type Map_component_type_name.t }
  type optimization_function = Setting.optimization_function

  type specification = ...

  type model = { universe : universe; config : configuration; spec : specification; optimization : optimization_function }
  
  val is_valid : model -> semantic_error list

  val translate_to_flat : model -> Aeolus_flat.model
  val translate_to_location : model -> Aeolus_location.model
  val translate_to_hierarchy : model -> Aeolus_hierarchy.model

  val string_of_configuration : configuration -> string
  val string_of_universe : universe -> string
  val string_of_model : model -> string
end


module Aeolus_location : sig
  ...
  type configuration = ...
  type universe = ...
  type model = ...
  type specification = ...
  type optimization_function = ...
  
  val is_valid : model -> semantic_error list

  val translate_to_flat : model -> Aeolus_flat.model
  val translate_to_location : model -> Aeolus_location.model
  val translate_to_hierarchy : model -> Aeolus_hierarchy.model

  val string_of_configuration : configuration -> string
  val string_of_universe : universe -> string
  val string_of_model : model -> string
end


module Aeolus_hierarchy : sig
  ...
  type configuration = ...
  type universe = ...
  type model = ...
  type specification = ...
  type optimization_function = ...
  
  val is_valid : model -> semantic_error list

  val translate_to_flat : model -> Aeolus_flat.model
  val translate_to_location : model -> Aeolus_location.model
  val translate_to_hierarchy : model -> Aeolus_hierarchy.model

  val string_of_configuration : configuration -> string
  val string_of_universe : universe -> string
  val string_of_model : model -> string
end
