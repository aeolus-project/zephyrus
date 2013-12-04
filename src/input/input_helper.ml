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
(*    along with Zephyrus.  If not, see <http=//www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on
    - Pervasives (standard library: files, opening and closing)
    - String     (standard library)
    - Yojson     (for json importing)
    - output/Zephyrus_log (for logging)
*)

let input_open filename = 
  try Some (filename, open_in filename)
  with Sys_error e -> (Zephyrus_log.log_input_file_error filename e; None)


let parse_json parser filename = 
  match input_open filename with
  | None -> None
  | Some(filename, file) -> 
    try Some (parser (Yojson.Safe.init_lexer ())  (Lexing.from_channel file))
    with Yojson.Json_error e -> (Zephyrus_log.log_input_file_error filename e; None)

let parse_standard parser lexer filename =
  match input_open filename with
  | None -> None
  | Some(filename, file) -> 
    try Some (parser lexer (Lexing.from_channel file))
    with Parsing.Parse_error -> (Zephyrus_log.log_input_file_error filename "does not have a valid syntax"; None)
