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
    - Unix
    - Filename
    - String
*)


(** 1. helpers on external commands. *)

exception Wrong_argument_number

type program = {
  name     : string;
  commands : string list;
  exe      : string list -> string; }
type pid = int

let program_did_exit_ok process_status = match process_status with 
  | Unix.WEXITED 0 -> true | _ -> false
  
let program_is_available program = List.fold_left (fun res cmd -> (program_did_exit_ok (Unix.system ("which " ^ cmd  ^ " > /dev/null"))) && res) true program.commands
let programs_are_available l = List.fold_left (fun res p -> (program_is_available p) && res) true l

let program_sync_exec  p l = 
(* DEBUG **************************) 
Zephyrus_log.log_execution (Printf.sprintf "executing \"%s\"\n" (p.exe l)); flush stdout;
Unix.system (p.exe l)

let program_async_exec p l = let id = Unix.fork () in if id <> 0 then id else Unix.execv "/bin/sh" [|"-c"; ("\"" ^ (p.exe l) ^ "\"") |]
let program_wait id = let (_, s) = Unix.waitpid [] id in s



let mzn2fzn = {
  name     = "G12 mzn2fzn";
  commands = ["mzn2fzn"];
  exe      = (fun l -> match l with [input; output] -> "mzn2fzn --no-output-ozn -o " ^ (String.escaped output) ^ " " ^ (String.escaped input)
                       | _ -> raise Wrong_argument_number)
}

let g12_flatzinc_solver = {
  name     = "G12";
  commands = ["flatzinc"];
  exe      = (fun l -> match l with [input; output] -> "flatzinc -o " ^ (String.escaped output) ^ " " ^ (String.escaped input)
                       | _ -> raise Wrong_argument_number)
}

let gecode_flatzinc_solver = {
  name     = "GeCode";
  commands = ["fz"];
  exe      = (fun l -> match l with [input; output] -> "fz -o " ^ (String.escaped output) ^ " " ^ (String.escaped input)
                       | _ -> raise Wrong_argument_number)
}

let g12_minizinc_solver = {
  name     = "G12";
  commands = ["mzn2fzn"; "flatzinc"];
  exe      = (fun l -> match l with [input; output] -> let flatzinc = Filename.temp_file "zephyrus_" ".fzn" in
    "mzn2fzn -o  " ^ flatzinc ^ " " ^ (String.escaped input) ^ " && " ^ "flatzinc -o " ^ (String.escaped output) ^ " " ^ flatzinc
                       | _ -> raise Wrong_argument_number)
}

let g12_cpx_minizinc_solver = {
  name     = "G12-cpx";
  commands = ["mzn-g12cpx"];
  exe      = (fun l -> match l with [input; output] -> "mzn-g12cpx --no-output-ozn -o " ^ (String.escaped output) ^ " " ^ (String.escaped input)
                       | _ -> raise Wrong_argument_number)
}

let gecode_minizinc_solver = {
  name     = "GeCode";
  commands = ["mzn2fzn"; "fz"];
  exe      = (fun l -> match l with [input; output] -> let flatzinc = Filename.temp_file "zephyrus_" ".fzn" in
    "mzn2fzn --no-output-ozn -o  " ^ flatzinc ^ " " ^ (String.escaped input) ^ " && " ^ "fz -o " ^ (String.escaped output) ^ " " ^ flatzinc
                       | _ -> raise Wrong_argument_number)
}


(** 2. Custom solver handling *)

(* Substrings in the custom solver command string which should be replaced by the input/output file path *)
let input_marker  = "<IN>"
let output_marker = "<OUT>"

(* Takes a command in form of a string and replaces all input/output markers by the provided input/output file path. *)
let concretize_command generic_command input_file_path output_file_path : string =
    let step0 = generic_command in
    let step1 = Str.global_replace (Str.regexp_string input_marker ) input_file_path  step0 in
    let step2 = Str.global_replace (Str.regexp_string output_marker) output_file_path step1 in
    step2

let make_minizinc_solver_of_custom_flatzinc_solver_command command = {
  name     = Printf.sprintf "Custom FlatZinc Solver (%s)" command;
  commands = [];
  exe      = function
             | [input; output] -> 
                 let flatzinc_file_path = Filename.temp_file "zephyrus_" ".fzn" in
                 let mzn2fzn_command_part  = 
                   begin
                     match Settings.get_custom_mzn2fzn_command () with
                     | Some command -> (concretize_command command) (String.escaped input) flatzinc_file_path
                     | None         -> Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" flatzinc_file_path (String.escaped input)
                   end in
                 let flatzinc_command_part = (concretize_command command) flatzinc_file_path (String.escaped output) in
                 Printf.sprintf "%s && %s" mzn2fzn_command_part flatzinc_command_part
             | _ -> raise Wrong_argument_number
}

let make_minizinc_solver_of_custom_minizinc_solver_command command = {
  name     = Printf.sprintf "Custom MiniZinc Solver (%s)" command;
  commands = [];
  exe      = function
             | [input; output] -> 
                 (concretize_command command) (String.escaped input) (String.escaped output)
             | _ -> raise Wrong_argument_number
}

(** 3. File name manipulation *)

type file = {dirname : string; basename : string; suffix : string}
let file_default = {dirname = ""; basename = ""; suffix = ""}

let file_process_name s = if s <> "" then (
    let dir_name  = Filename.dirname s in
    let base_name_tmp = Filename.basename s in
     let (index, len) = (String.rindex base_name_tmp '.', String.length base_name_tmp) in
      if (0 <= index) && (index < len) then
       { dirname = dir_name; basename = String.sub base_name_tmp 0 index; suffix = String.sub base_name_tmp index (len - index) }
      else { dirname = dir_name; basename = base_name_tmp; suffix = "" }
  ) else file_default

let keep_file current_name file = current_name
(*
  let basename = Filename.basename current_name in
  let filename = file.dirname ^ Filename.dir_sep ^ basename in
  (if current_name != filename then Unix.rename current_name filename); filename
*)

let file_create keep file = let filename_tmp = Filename.temp_file file.basename file.suffix in
  if keep then keep_file filename_tmp file else filename_tmp

let file_print keep file s = 
  let (filename_tmp, out_c) = Filename.open_temp_file file.basename file.suffix in
  output_string out_c s; flush out_c; close_out out_c;
  if keep then keep_file filename_tmp file else filename_tmp

(*
let file_create ?(keep: bool = false) file = let filename_tmp = Filename.temp_file file.basename file.suffix in
  if keep then let filename = file.basename ^ filename_tmp in Unix.rename filename_tmp filename; filename else filename_tmp

let file_print ?(keep: bool = false) file s = 
  let (filename_tmp, out_c) = Filename.open_temp_file file.basename file.suffix in
  output_string out_c s; flush out_c; close_out out_c;
  if keep then let filename = file.basename ^ filename_tmp in Unix.rename filename_tmp filename; filename else filename_tmp
*)
