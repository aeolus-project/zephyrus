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

(** Some common data structures and operations on them. *)

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

(** {2 Custom and extended versions of standard library modules.} *)

(** Wrapper module for integer values to use with the [Set] and [Map] modules.
    It has the same type as [Set.OrderedType] and [Map.OrderedType]. *)
include module type of Data_common_int
(** Custom extension of the [String] module from the standard library. *)
include module type of Data_common_string

(** Custom extension of the [List] module from the standard library. *)
include module type of Data_common_list

(** Custom extension of the [Set] module from the standard library with construction and conversion. *)
include module type of Data_common_set 
(** Custom extension of the [Map] module from the standard library with construction, conversion and extraction. *)
include module type of Data_common_map 


(** {2 Some basic tools used mostly for managing identifiers and mappings between them.} *)

(** Unique identifier management *)
include module type of Data_common_unique_id

(** One-way mappings. *)
include module type of Data_common_mapping

(** Catalogs: two-way mappings. *)
include module type of Data_common_catalog