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

open Data_model
open Printf

let default_package_version = 1

let package (convert_package_id : package_id -> string) (package_field : string) (package : package) =
  
  (* Package field. *)
  let package_field_lines = [sprintf "package: %s" package_field] in
  
  (* Package version field. *)
  (* As we already have expanded versions this field is in fact useless, but CUDF requires it. *)
  let package_version_field_lines = [sprintf "version: %d" default_package_version] in
  
  (* Package depends field. *)
  let package_depends_field_lines =
    let depend' : string list list = 
      List.map (fun package_id_set ->
        List.map convert_package_id (Package_id_set.elements package_id_set)
      ) (Package_id_set_set.elements package#depend) in
    let depends_string = String.concat ", " (List.map (String.concat " | ") depend') in
    if depends_string = "" 
    then []
    else [sprintf "depends: %s" depends_string] in

  (* Package conflicts field. *)
  let package_conflicts_field_lines =
    let conflict' : string list =
      List.map convert_package_id (Package_id_set.elements package#conflict) in
    let conflicts_string = String.concat ", " conflict' in
    if conflicts_string = ""
    then []
    else [sprintf "conflicts: %s" conflicts_string] in
  
  (* Putting all the fields together. *)
  String.concat "\n" (
      package_field_lines
    @ package_version_field_lines
    @ package_depends_field_lines
    @ package_conflicts_field_lines)

let repository convert_package_id universe repository_id =

  (* let convert_package_id = Name_of.package_id in *)
  (* let convert_package_id = String_of.package_id in *)
  
  let convert_package package_id =
    package convert_package_id (convert_package_id package_id) (universe#get_package package_id) in
  
  let convert_repository repository_id =
    let repository = universe#get_repository repository_id in
    let package_ids = repository#package_ids in
    
    (* Convert every package which belongs to the repository. *)
    let package_strings = List.map convert_package (Package_id_set.elements package_ids) in
  
    (* Separate all the package descriptions with an empty line. *)
    let repository_string = String.concat "\n\n" package_strings in
  
    (* One additional end-of-line at the end of the CUDF file is required. *)
    sprintf "%s\n" repository_string in

   (* Do it. *)
   convert_repository repository_id