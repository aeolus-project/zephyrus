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


(** 1. Resources *)
let resource_name name = name
let resource_id id     = string_of_int id

let string_of_resource_provide_arity resource_provide_arity = string_of_int resource_provide_arity
let string_of_resource_consume_arity resource_consumption = string_of_int resource_consumption

(** 2. Component types *)
let string_of_component_type_name component_type_name = component_type_name
let string_of_port_name port_name = port_name
let string_of_provide_arity provide_arity = match provide_arity with | Data_model.Infinite_provide -> "infinite" | Data_model.Finite_provide i -> string_of_int i
let string_of_require_arity require_arity = string_of_int require_arity

(** 3. Packages *)
let string_of_package_name package_name = package_name

(** 4. Repositories *)
let string_of_repository_name repository_name = repository_name

(** 5. Location *)
let string_of_location_name location_name = location_name
let string_of_component_name component_name = component_name

let string_of_spec_variable_name spec_variable_name = spec_variable_name
let string_of_spec_const spec_const = string_of_int spec_const





