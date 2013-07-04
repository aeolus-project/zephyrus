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
    - input/Settings (for configuration)
    - output/Zephyrus_log (for logging)
*)

let input_open filename = try Some (filename, open_in filename) with
    | Sys_error e -> (Zephyrus_log.log_input_file_error filename e; None)


let parse_json parser filename = match input_open filename with
  | None -> None
  | Some(filename, file) -> try Some (parser (Yojson.Safe.init_lexer ())  (Lexing.from_channel file)) with
    | Yojson.Json_error e -> (Zephyrus_log.log_input_file_error filename e; None)

let parse_standard parser lexer filename = match input_open filename with
  | None -> None
  | Some(filename, file) -> try Some (parser lexer (Lexing.from_channel file)) with
    | Parsing.Parse_error -> (Zephyrus_log.log_input_file_error filename "does not have a valid syntax"; None)


(*
let string_of_file filename =
  let file' = open_in filename in match file with None -> None | Some(file) -> let length = in_channel_length file in
  if length > Sys.max_string_length then (* Check if the content of the channel will fit in a single string... *)
    (Zephyrus_log.log_input_file_error filename  "too big to be parsed"; None)
  else let s = String.create length in really_input file s 0 length; close_in file; (Some(s))




let input_and_set desc input output fun_set = match !input with
  | None           -> Zephyrus_log.log_setting_not_set ("input file for \"" ^ desc ^ "\"")
  | Some(filename) -> match input_open filename with
    | Input_file_error -> ()
    | Input_file(file) -> match string_of_input_channel filename file with
      | Input_string_error -> ()
      | Input_string(str)  -> try output := (Some (fun_set str)) with
        | Parsing.Parse_error -> Zephyrus_log.log_input_file_error filename  "does not have a valid syntax"
*)

