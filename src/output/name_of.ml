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
    - datatypes/Data_helper
*)

(** The global catalog of id <-> name correspondence. *)


(*let get_catalog ()      = !Data_state.catalog_full*)
let monad (f1, f2) v = try f2 v with | Not_found -> f1 v

let resource_f       = (string_of_int, (fun x -> Abstract_io.String_of.resource_name (Printf.printf "DEBUG: Name_of 40\n"; flush stdout; Data_state.resource x)))
let component_type_f = (string_of_int, (fun x -> Abstract_io.String_of.component_type_ref (Data_state.component_type x)))
let port_f           = (string_of_int, (fun x -> Abstract_io.String_of.port_name (Data_state.port x)))
let package_f        = (string_of_int, (fun x -> Abstract_io.String_of.package_name (Data_state.package x)))
let repository_f     = (string_of_int, (fun x -> Abstract_io.String_of.repository_name (Data_state.repository x)))
let location_f       = (string_of_int, (fun x -> Abstract_io.String_of.location_name (Data_state.location x)))
let component_f      = (string_of_int, (fun x -> Abstract_io.String_of.component_name (Data_state.component x)))


let resource_id       id = monad resource_f id
let component_type_id id = monad component_type_f id
let port_id           id = monad port_f id
let package_id        id = monad package_f id
let repository_id     id = monad repository_f id
let location_id       id = monad location_f id
let component_id      id = monad component_f id



