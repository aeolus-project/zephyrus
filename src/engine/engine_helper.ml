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

let string_of_process_status = 
  function
  | Unix.WEXITED   status -> Printf.sprintf "%s %d" "WEXITED"   status
  | Unix.WSIGNALED status -> Printf.sprintf "%s %d" "WSIGNALED" status
  | Unix.WSTOPPED  status -> Printf.sprintf "%s %d" "WSTOPPED"  status

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

let kill pid =
    try
      Zephyrus_log.log_execution (Printf.sprintf "Killing process %d...\n%!" pid);
      Unix.kill pid 9;
      Zephyrus_log.log_execution (Printf.sprintf "Killed process %d!\n%!" pid);
    with
      Unix.Unix_error (Unix.ESRCH, "kill", _) -> 
        Zephyrus_log.log_execution (Printf.sprintf "Process %d already dead!\n%!" pid);
        ()

let make_zephyrus_temp_file file_extension =
  Filename.temp_file "zephyrus-" file_extension

let mzn2fzn = {
  name     = "G12 MiniZinc to FlatZinc converter";
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
  name     = "G12 FlatZinc Solver";
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
  name     = "GeCode FlatZinc solver";
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
  name     = "G12 MiniZinc Solver"; (* (using the G12 MiniZinc to FlatZinc converter) *)
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

let gecode_minizinc_solver = {
  name     = "GeCode MiniZinc Solver"; (* (using the G12 MiniZinc to FlatZinc converter) *)
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

let g12_cpx_minizinc_solver = {
  name     = "G12 CPX MiniZinc Solver"; (* (using the G12 MiniZinc to FlatZinc converter) *)
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


(** 4. The portfolio method. *)

let portfolio (programs : program list) (verify_output_file : string -> bool) (input_file : string) (output_file : string) : bool =

  Zephyrus_log.log_execution (Printf.sprintf "Portfolio: BEGINNING...%!\n");
  
  (* First stage : launch asynchronously all the portfolio programs. *)

  Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Running all portfolio programs...%!\n");

  (* The list [running_programs] will be filled with all the information about the programs that are launched. *)
  let running_programs : (int * (program * string * string)) list = 
    List.mapi (fun index (program : program) ->

      (* Prepare the program arguments *)
      let program_input_file  : string = input_file in
      let program_output_file : string = Filename.temp_file "zephyrus_portfolio_" ".output" in
      let args = [program_input_file; program_output_file] in

      (* Get the program command (for debug purposes). *)
      let command : string = program.exe args in

      (* Run the program asynchronously. *)
      Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Launching asynchronously program (%d): \"%s\"...\n%!" (index + 1) program.name);
      let pid = program_async_exec program args in
      Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Program \"%s\" has been launched at pid %d !\n%!" program.name pid);

      (* Return the program information. *)
      (pid, (program, command, program_output_file))

    ) programs in

  Zephyrus_log.log_execution (Printf.sprintf "Portfolio: All portfolio programs have been launched:\n%!");
  
  let separator = "+-------" in

  let program_descriptions =
    List.mapi (fun index (pid, (program, command, program_output_file)) -> 
      let lines = [
        Printf.sprintf "| -( %d )-"    (index + 1);
        Printf.sprintf "| Program: %s" program.name;
        Printf.sprintf "| PID:     %d" pid;
        Printf.sprintf "| Command: %s" command;
      ] in
      String.concat "\n" lines
    ) running_programs in

  Zephyrus_log.log_execution (Printf.sprintf "%s\n%s\n%s\n" separator (String.concat ("\n" ^ separator ^ "\n") program_descriptions) separator);

  (* Second stage : wait for results. *)

  (* Prepare the inital state. *)
  let processes_left = ref running_programs in
  let the_first_correct_one  = ref None in

  (* Until we find a solution, if there are still any processes left, wait for one of them to terminate... *)
  while (!the_first_correct_one = None) && (List.length !processes_left > 0) do
    Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Waiting for processes...\n%!");
    let (pid, termination_status) = Unix.wait () in

    (* Get the program information corresponding to the process that has just terminated. *)
    try 
      let (program, command, program_output_file) = List.assoc pid running_programs in

      Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) has terminated with status %s\n%!" pid program.name (string_of_process_status termination_status));

      (* Remove the process from the list of processes running. *)
      processes_left := List.remove_assoc pid !processes_left;
      
      (* Check if it terminated correctly... *)
      if did_program_exit_ok termination_status
      then begin
        Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) termination status OK!\n%!" pid program.name);

        (* Check if its output is acceptable... *)
        if verify_output_file program_output_file
        then (
          Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) output file OK!\n%!" pid program.name);
          
          (* This is the first process which has terminated correctly and produced a correct solution! *)
          Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) is the portfolio WINNER\n%!" pid program.name);

          (* Mark the process as the first correct one. *)
          the_first_correct_one := Some(program, command, program_output_file);

          (* Now kill all the other still running processes. *)
          let processes_left_pids = fst (List.split !processes_left) in
          Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Found the WINNER, killing the others...\n%!");
          List.iter kill processes_left_pids;

          (* After the killing there are no processes left running. *)
          processes_left := []
        )
        else 
          Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) output file (%s) wrong\n%!" pid program.name program_output_file);
          ()
      end else begin
        Zephyrus_log.log_execution (Printf.sprintf "Portfolio: Process %d (program: %s) termination status wrong\n%!" pid program.name);
        ()
      end

    with Not_found -> 
      Zephyrus_log.log_execution (Printf.sprintf "Portfolio: A child process with pid %d, which was not created by the portfolio, has just terminated!\n%!" pid)

  done;

  (* Check if we have found a solution... *)
  match !the_first_correct_one with
  | Some (program, command, program_output_file) -> 
     Zephyrus_log.log_execution (Printf.sprintf "Portfolio: END! The portfolio WINNER is the program '%s' with the output file '%s'!\n%!" program.name program_output_file);

     (* Copy the program's output file to the desired output file location. *)
     Input_helper.file_copy program_output_file output_file;

     (* Return success. *)
     true

  | None -> 
      Zephyrus_log.log_execution (Printf.sprintf "Portfolio: END! No correct solution was found!\n%!");

      (* Return failure. *)
      false

let portfolio_test () =
  Printf.printf "START!\n";

  let input_file  = "/tmp/portfolio.in" in 
  let output_file = "/tmp/portfolio.out" in 

  let make_program i = {
    name     = Printf.sprintf "Sleeper %d" i;
    commands = ["sleep"; "echo"];
    exe      = (fun args -> 
                  match args with 
                  | [input; output] -> 
                    let in_file  = String.escaped input in
                    let out_file = String.escaped output in
                    Printf.sprintf "sleep %d && echo \"done %d!\" > %s" i i out_file
                  | _ -> raise Wrong_argument_number)
  } in

  let rec i_list i = 
    if i = 0 
    then []
    else i::(i_list (i - 1)) in

  let programs : program list = List.map (fun i -> make_program i) (i_list 9) in

  let validate_output_file output_filename = 
    Input_helper.lines_of_file output_filename = ["done 6!"] in

  let result = portfolio programs validate_output_file input_file output_file in

  (match result with
  | true  -> Printf.printf "Success reported!\n" 
  | false -> Printf.printf "Failure reported!\n");

  let lines = Input_helper.lines_of_file output_file in
  Printf.printf "Output file %s\n------\n%s\n------\n" output_file (String.concat "\n" lines);

  Printf.printf "END!\n";
  exit 0