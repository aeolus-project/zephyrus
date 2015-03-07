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
let catalog_full : Data_model_catalog.closed_model_catalog option ref = ref None

let set_catalog catalog = catalog_full := catalog
let get_catalog ()      = !catalog_full

let resource_f       = (string_of_int, (fun catalog x -> Abstract_io.String_of.resource_name (catalog#resource#name_of_id x))       )
let component_type_f = (string_of_int, (fun catalog x -> Abstract_io.String_of.component_type_ref (catalog#component_type#name_of_id x)) )
let port_f           = (string_of_int, (fun catalog x -> Abstract_io.String_of.port_name (catalog#port#name_of_id x))          )
let package_f        = (string_of_int, (fun catalog x -> let (_, y) = catalog#package#name_of_id x in Abstract_io.String_of.package_name y))
let repository_f     = (string_of_int, (fun catalog x -> Abstract_io.String_of.repository_name (catalog#repository#name_of_id x))    )
let location_f       = (string_of_int, (fun catalog x -> Abstract_io.String_of.location_name (catalog#location#name_of_id x))      )
let component_f      = (string_of_int, (fun catalog x -> Abstract_io.String_of.component_name (catalog#component#name_of_id x))     )

let object_name_of_id object_f =
  match get_catalog () with
  | None         -> (fst object_f)
  | Some catalog -> (* TODO: This is a dirty hack, we should just handle deprecated stuff better. *)
                    (fun id -> if id = -1 then "DEPRECATED" else ((snd object_f) catalog) id)

let resource_id       id = object_name_of_id resource_f id
let component_type_id id = object_name_of_id component_type_f id
let port_id           id = object_name_of_id port_f id
let package_id        id = object_name_of_id package_f id
let repository_id     id = object_name_of_id repository_f id
let location_id       id = object_name_of_id location_f id
let component_id      id = object_name_of_id component_f id



