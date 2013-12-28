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
    -Data_common_int
    -Data_common_string
    -Data_common_list
    -Data_common_set
    -Data_common_map
    -Data_common_unique_id
    -Data_common_mapping
    -Data_common_catalog
*)

(** 1. Custom and extended versions of standard library modules. *)

(** Custom Int and String modules. *)
include module type of Data_common_int
include module type of Data_common_string

(** Custom List module. *)
include module type of Data_common_list

(** Custom sets and maps. *)
include module type of Data_common_set (** Extension of the Set module from the standard library with construction and conversion. **)
include module type of Data_common_map (** Extension of the Map module from the standard library with construction, conversion and extraction. **)


(** 2. Some basic tools used mostly for managing ids and mappings between them. *)

(** Unique identifier management *)
include module type of Data_common_unique_id

(** One-way mappings. *)
include module type of Data_common_mapping

(** Catalogs: two-way mappings. *)
include module type of Data_common_catalog