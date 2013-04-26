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



(************************************)
(** 1/6 Basic Types                 *)
(************************************)

type component_type_name = string
type port_name           = string
type component_name      = string
type package_name        = string
type repository_name     = string
type location_name       = string
type resource_name       = string
type spec_variable_name  = string

type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

let string_of_element element =
  match element with
  | ComponentType (component_type_name) -> component_type_name
  | Port          (port_name)           -> port_name
  | Package       (package_name)        -> package_name

