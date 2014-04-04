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
  exe      : string list -> string;
}

type pid = int

let did_program_exit_ok process_status = 
  match process_status with 
  | Unix.WEXITED 0 -> true 
  | _              -> false
  
let program_is_available program = 
  List.for_all (fun command -> 
    let which_command = Printf.sprintf "which %s > /dev/null" command in
    did_program_exit_ok (Unix.system which_command)
  ) program.commands

let programs_are_available programs = 
  List.for_all program_is_available programs

let execv_run_bash_command command =
    Zephyrus_log.log_execution (Printf.sprintf "Running a bash command at pid %d: /bin/bash -c %s\n%!" (Unix.getpid ()) command);
    Unix.execv "/bin/bash" [|""; "-c"; command|]

let program_async_exec program args = 
  flush_all (); (* Before a fork we flush all the open output channels, if not the waiting data may get duplicated and some things may end up printed multiple times. *)
  let pid = Unix.fork () in
  if pid = 0
  then 
    (* The child process: run the program. *)
    let command = program.exe args in
    execv_run_bash_command command
  else
    (* The parent process: return the child's pid. *)
    pid

let program_wait_pid pid =
  let (_, termination_status) = Unix.waitpid [] pid in
  termination_status

let program_wait () =
  let (pid, termination_status) = Unix.wait () in
  (pid, termination_status)

let program_sync_exec program args = 
  let pid = program_async_exec program args in
  program_wait_pid pid


let make_zephyrus_temp_file file_extension =
  Filename.temp_file "zephyrus-" file_extension

let mzn2fzn = {
  name     = "G12 mzn2fzn";
  commands = ["mzn2fzn"];
  exe      = (fun args -> 
                match args with 
                | [input; output] -> 
                  let mzn_file = String.escaped input in
                  let fzn_file = String.escaped output in
                  Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" fzn_file mzn_file
                | _ -> raise Wrong_argument_number)
}

let g12_flatzinc_solver = {
  name     = "G12";
  commands = ["flatzinc"];
  exe      = (fun args -> 
                match args with
                | [input; output] -> 
                  let fzn_file = String.escaped input in
                  let sol_file = String.escaped output in
                  Printf.sprintf "flatzinc -o %s %s" sol_file fzn_file
                | _ -> raise Wrong_argument_number)
}

let gecode_flatzinc_solver = {
  name     = "GeCode";
  commands = ["fz"];
  exe      = (fun args -> 
                match args with 
                | [input; output] -> 
                  let fzn_file = String.escaped input in
                  let sol_file = String.escaped output in
                  Printf.sprintf "fz -o %s %s" sol_file fzn_file
                | _ -> raise Wrong_argument_number)
}

let g12_minizinc_solver = {
  name     = "G12";
  commands = ["mzn2fzn"; "flatzinc"];
  exe      = (fun args -> 
                match args with 
                | [input; output] -> 
                  let mzn_file = String.escaped input in
                  let sol_file = String.escaped output in
                  let fzn_file = make_zephyrus_temp_file ".fzn" in 
                  let mzn2fzn_command_part  = Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s"  fzn_file mzn_file in
                  let flatzinc_command_part = Printf.sprintf "flatzinc -o %s %s" sol_file fzn_file in
                  Printf.sprintf "%s && %s" mzn2fzn_command_part flatzinc_command_part
                | _ -> raise Wrong_argument_number)
}

let g12_cpx_minizinc_solver = {
  name     = "G12-cpx";
  commands = ["mzn2fzn"; "mzn-g12cpx"];
  exe      = (fun args -> 
                match args with 
                | [input; output] ->
                  let mzn_file = String.escaped input in
                  let sol_file = String.escaped output in
                  let fzn_file = make_zephyrus_temp_file ".fzn" in
                  let mzn2fzn_command_part  = Printf.sprintf "mzn2fzn --no-output-ozn -G g12_cpx -o %s %s" fzn_file mzn_file in
                  let flatzinc_command_part = Printf.sprintf "fzn_cpx -s %s > %s"                          fzn_file sol_file in
                  Printf.sprintf "%s && %s" mzn2fzn_command_part flatzinc_command_part
                | _ -> raise Wrong_argument_number)
}

let gecode_minizinc_solver = {
  name     = "GeCode";
  commands = ["mzn2fzn"; "fz"];
  exe      = (fun args -> 
                match args with 
                | [input; output] -> 
                  let mzn_file = String.escaped input in
                  let sol_file = String.escaped output in
                  let fzn_file = make_zephyrus_temp_file ".fzn" in 
                  let mzn2fzn_command_part  = Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s"  fzn_file mzn_file in
                  let flatzinc_command_part = Printf.sprintf "fz -o %s %s" sol_file fzn_file in
                  Printf.sprintf "%s && %s" mzn2fzn_command_part flatzinc_command_part
                | _ -> raise Wrong_argument_number)
}


(** 2. Custom solver handling *)

(* Substrings in the custom solver command string which should be replaced by the input/output file path *)
let input_marker  = "<IN>"
let output_marker = "<OUT>"

(* [concretize_command generic_command ~input_file_path ~output_file_path] takes a command in form of a string and replaces all input/output markers by the provided input/output file path. *)
let concretize_command generic_command ~input_file_path ~output_file_path : string =
    let step0 = generic_command in
    let step1 = Str.global_replace (Str.regexp_string input_marker ) input_file_path  step0 in
    let step2 = Str.global_replace (Str.regexp_string output_marker) output_file_path step1 in
    step2


let make_minizinc_solver_of_custom_flatzinc_solver_command command = {
  name     = Printf.sprintf "Custom FlatZinc Solver (%s)" command;
  commands = [];
  exe      = (fun args ->
               match args with
               | [input; output] -> 
                 let mzn_file = String.escaped input in
                 let sol_file = String.escaped output in
                 let fzn_file = make_zephyrus_temp_file ".fzn" in
                 let mzn2fzn_command_part  = 
                   begin
                     match Settings.get_custom_mzn2fzn_command () with
                     | Some command -> (concretize_command command) mzn_file fzn_file
                     | None         -> Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" fzn_file mzn_file
                   end in
                 let flatzinc_command_part = (concretize_command command) fzn_file sol_file in
                 Printf.sprintf "%s && %s" mzn2fzn_command_part flatzinc_command_part
               | _ -> raise Wrong_argument_number)
}

let make_minizinc_solver_of_custom_minizinc_solver_command command = {
  name     = Printf.sprintf "Custom MiniZinc Solver (%s)" command;
  commands = [];
  exe      = (fun args ->
               match args with
               | [input; output] -> 
                 let mzn_file = String.escaped input in
                 let sol_file = String.escaped output in
                 (concretize_command command) mzn_file sol_file
               | _ -> raise Wrong_argument_number)
}

(** 3. File name manipulation *)

type file = {dirname : string; basename : string; suffix : string}
let file_default = {dirname = ""; basename = ""; suffix = ""}

let file_process_name s = 
  if s <> ""
  then (
    let dir_name  = Filename.dirname s in
    let base_name_tmp = Filename.basename s in
     let (index, len) = (String.rindex base_name_tmp '.', String.length base_name_tmp) in
      if (0 <= index) && (index < len) 
      then {
        dirname = dir_name;
        basename = String.sub base_name_tmp 0 index;
        suffix = String.sub base_name_tmp index (len - index) 
      } else { 
        dirname = dir_name;
        basename = base_name_tmp;
        suffix = "" 
      }) 
  else file_default

let keep_file current_name file = current_name

let file_create keep file = let filename_tmp = Filename.temp_file file.basename file.suffix in
  if keep then keep_file filename_tmp file else filename_tmp

let file_print keep file s = 
  let (filename_tmp, out_c) = Filename.open_temp_file file.basename file.suffix in
  output_string out_c s; flush out_c; close_out out_c;
  if keep then keep_file filename_tmp file else filename_tmp
