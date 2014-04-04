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

open Engine_helper

let test () =
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