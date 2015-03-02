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

let lines_of_file filename = 
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true; do
      lines := input_line channel :: !lines
    done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let copy_buffer_size = 8192

let file_copy input_filename output_filename = 
  let open Unix in
  let buffer = String.create copy_buffer_size in
  let fd_in = openfile input_filename [O_RDONLY] 0 in
  let fd_out = openfile output_filename [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () = 
    match read fd_in buffer 0 copy_buffer_size with
    | 0 -> ()
    | r -> ignore (write fd_out buffer 0 r);
           copy_loop () in 
  copy_loop ();
  close fd_in;
  close fd_out


