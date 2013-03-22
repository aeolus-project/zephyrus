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


let string_of_printing_function printing_function argument =
  let (tmp_file, out_channel) = Filename.open_temp_file "tmp_string_of_" "" in
  printing_function out_channel argument;
  close_out out_channel;
  let in_channel = open_in tmp_file in
  let s = input_line in_channel in
  close_in in_channel;
  Sys.remove tmp_file; 
  s

let lines_of_strings strings = String.concat "\n" strings

let ident = "  "

let indent_line  line  = Printf.sprintf "%s%s" ident line
let indent_lines lines = List.map indent_line lines

let indent_lines_of_strings strings = lines_of_strings (indent_lines strings)

let string_of_input_channel (in_channel : in_channel) =
  let in_channel_length = in_channel_length in_channel in
  if in_channel_length > Sys.max_string_length
  then failwith "The input channel contents do not fit in a single string!"
  else
    let s = String.create in_channel_length in
    really_input in_channel s 0 in_channel_length;
    close_in in_channel;
    s

(* Helper for handling errors of external commands. *)
let did_process_exit_ok process_status =
  match process_status with 
  | Unix.WEXITED 0 -> true 
  | _ -> false
  

type in_out_program = {
  name    : string;
  command : string;
  exe     : string -> string -> string;
}

let is_program_available program =
  did_process_exit_ok (Unix.system (Printf.sprintf "which %s > /dev/null" program))

let check_if_programs_available programs =
  (* Check if given commands are available on the machine. *)
  List.iter (fun program ->
    if not (is_program_available program.command)
    then failwith (Printf.sprintf "The program \"%s\" (command \"%s\") is not available on this machine!" program.name program.command)
  ) programs