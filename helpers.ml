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

open ExtLib

module List = 
  struct
    include ExtLib.List

    let flatten_map f l = List.flatten (List.map f l)
  end

let string_of_printing_function printing_function argument =
  (* Prepare a temporary file and open it for writing. *)
  let (tmp_file, out_channel) = Filename.open_temp_file "tmp_string_of_" "" in
  (* Print to that file. *)
  printing_function out_channel argument;
  close_out out_channel;
  (* Read from the file. *)
  let in_channel = open_in tmp_file in
  let s = input_line in_channel in
  close_in in_channel;
  (* Delete the temporary file. *)
  Sys.remove tmp_file;
  (* Return. *)
  s

let lines_of_strings strings = String.concat "\n" strings

let ident = "  "

let indent_line  line  = Printf.sprintf "%s%s" ident line
let indent_lines lines = List.map indent_line lines

let indent_lines_of_strings strings = lines_of_strings (indent_lines strings)

let string_of_input_channel (in_channel : in_channel) =
  (* Check if the content of the channel will fit in a single string... *)
  let in_channel_length = in_channel_length in_channel in
  if in_channel_length > Sys.max_string_length
  then 
    (* No it will not! *)
    failwith "The input channel contents do not fit in a single string!"
  else
    (* Yes, it will fit in a single string, we can proceed and read from the channel. *)
    let s = String.create in_channel_length in
    really_input in_channel s 0 in_channel_length;
    close_in in_channel;
    s

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


module Set_of_list =
  functor (S : Set.S) ->
  struct

    exception Double_element of S.elt

    let translate el_translate l =
      List.fold_left (fun set el ->
        let el = el_translate el
        in
        if S.mem el set
        then raise (Double_element el)
        else S.add el set
      ) S.empty l

  end

module Map_of_assoc_list =
  functor (M : Map.S) ->
  struct

    exception Double_key of M.key

    let translate key_translate value_translate l =
      List.fold_left (fun map (key, value) ->
        let key   = key_translate key
        and value = value_translate value
        in
        if M.mem key map
        then raise (Double_key key)
        else M.add key value map
      ) M.empty l

  end

module Map_of_list =
  functor (M : Map.S) ->
  struct

    exception Double_key of M.key

    let translate key_translate value_translate l =
      let module T = Map_of_assoc_list(M) in
      T.translate key_translate value_translate (List.combine l l)

  end

module Set_of_map_values =
  functor (M : Map.S) ->
  functor (Set : Set.S) ->
  struct

    exception Double_value of Set.elt

    let set_of_map_values map =
      M.fold (fun _ value set ->
        if Set.mem value set
        then raise (Double_value value)
        else Set.add value set
      ) map Set.empty

  end

module Set_of_map_keys =
  functor (Map : Map.S) ->
  functor (Set : Set.S with type elt = Map.key) ->
  struct

    let set_of_map_keys map =
      Map.fold (fun key _ set ->
        Set.add key set
      ) map Set.empty

  end

module Set_of_set =
  functor (Set_origin : Set.S) ->
  functor (Set_target : Set.S) -> 
  struct
    
    let convert f s = Set_origin.fold (fun v res -> Set_target.add (f v) res) s Set_target.empty
  
  end