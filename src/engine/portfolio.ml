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

(** Portfolio method of running worker-processes (run many processes trying to find a solution in parallel, get the first correct solution, kill remaining processes). *)

let string_of_process_status = 
  function
  | Unix.WEXITED   status -> Printf.sprintf "%s %d" "WEXITED"   status
  | Unix.WSIGNALED status -> Printf.sprintf "%s %d" "WSIGNALED" status
  | Unix.WSTOPPED  status -> Printf.sprintf "%s %d" "WSTOPPED"  status

let run_bash_command command =
    Printf.printf "Bash command running: pid %d : /bin/bash -c %s\n%!" (Unix.getpid ()) command;
    Unix.execv "/bin/bash" [|"";"-c";command|]

let portfolio (input_file : string) (output_file : string) (programs : (string -> string -> string) list) (verify_output_file : string -> bool) : string =
  
  Printf.printf "Running programs!%!\n";

  let running_programs : (int * (string * string)) list = 
    List.map (fun (program : string -> string -> string) ->
      let program_input_file  : string = input_file in
      let program_output_file : string = Filename.temp_file "zephyrus_portfolio_" ".output" in
      let command = program program_input_file program_output_file in
      flush_all (); (* Before running fork we flush all the open output channels, if not they may be duplicated and some things may get printed multiple times. *)
      let pid : int = Unix.fork () in
      if pid = 0
      then 
        (* The child process: run the program. *)
        run_bash_command command
      else
        (* The parent process: continue with the map function. *)
        (pid, (command, program_output_file))
    ) programs in

  Printf.printf "Programs are running!\n%!";

  let processes_left = ref running_programs in
  let the_right_one  = ref None in
  
  let kill pid =
    try
      Printf.printf "Killing process %d...\n%!" pid;
      Unix.kill pid 9;
      Printf.printf "Killed process %d!\n%!" pid;
    with
      Unix.Unix_error (Unix.ESRCH, "kill", _) -> 
        Printf.printf "Process %d already dead!\n%!" pid;
        () in

  while (List.length !processes_left > 0) && (!the_right_one = None) do
    Printf.printf "Waiting...\n%!";
    let (pid, termination_status) = Unix.wait () in
    let (command, program_output_file) = List.assoc pid running_programs in
    Printf.printf "Process %d (command: %s) has terminated with status %s\n%!" pid command (string_of_process_status termination_status);
    processes_left := List.remove_assoc pid !processes_left;
    match termination_status with
    | Unix.WEXITED 0 ->
      Printf.printf "Process %d (command: %s) termination status ok\n%!" pid command;
      if verify_output_file program_output_file
      then (
        Printf.printf "Process %d (command: %s) output file ok\n%!" pid command;
        Printf.printf "Process %d (command: %s) IS THE RIGHT ONE\n%!" pid command;
        the_right_one := Some(command, program_output_file);
        let processes_left_pids = fst (List.split !processes_left) in
        Printf.printf "Found THE RIGHT ONE, killing the others...\n%!";
        List.iter kill processes_left_pids;
        ()
      )
      else 
        Printf.printf "Process %d (command: %s) output file wrong\n%!" pid command;
        ()
    | _ -> 
      Printf.printf "Process %d (command: %s) termination status wrong\n%!" pid command;
      ()
  done;

  match !the_right_one with
  | Some (command, program_output_file) -> Printf.sprintf "Command: '%s', output file: '%s'." command program_output_file
  | None -> "No right one!"


let test () =
  Printf.printf "START!\n";

  let input_file  = "/tmp/portfolio.in" in 
  let output_file = "/tmp/portfolio.out" in 

  let make_program i input_file output_file =  Printf.sprintf "sleep %d && echo \"done %d!\" > %s" i i output_file in

  let rec i_list i = 
    if i = 0 
    then []
    else i::(i_list (i - 1)) in

  let programs : (string -> string -> string) list = List.map (fun i -> make_program i) (i_list 9) in

  let result = portfolio input_file output_file programs (fun output_file -> (Input_helper.lines_of_file output_file) = ["done 6!"]) in

  Printf.printf "%s\n" result; 

  Printf.printf "END!\n";
  exit 0