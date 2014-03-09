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
    - input/Settings (to know what to log)
    - output/Output_helper
*)

(* TODO: replace printfs by functions in output_helper.ml *)

let out_channel = stdout

(* core logging *)
let log_panic str = Output_helper.println out_channel ("Zephyrus panic: " ^ str ^ "\n\nExiting\n"); flush out_channel; exit(-1)
let log_error str = Output_helper.println out_channel ("Zephyrus error: " ^ str ^ "\n\nExiting\n"); flush out_channel; exit(-1)
let log_warning str = Output_helper.print out_channel ("Zephyrus warning: " ^ str); flush out_channel
let log_normal str = Output_helper.print out_channel str

(* stage logging *)
module Stage =
struct

  type stage_description = {
    stage_name       : string;       (**  *)
    stage_start_time : float option; (**  *)
    stage_end_time   : float option;
  }

  type stage_stack = (stage_description list) ref
  
  let empty_stage_stack () : stage_stack = ref []

  let make_new_stage (name : string) : stage_description = {
    stage_name       = name;
    stage_start_time = None;
    stage_end_time   = None;
  }

  let stage_name (stage : stage_description) : string = stage.stage_name

  let stage_duration (stage : stage_description) : float = 
    match stage.stage_start_time, stage.stage_end_time with
    | Some start_time, Some end_time -> end_time -. start_time
    | _                              -> 0.0

  let start_stage (stage_stack : stage_stack) (stage : stage_description) : unit =
    let start_time = Sys.time () in
    let started_stage = {
      stage_name       = stage.stage_name;
      stage_start_time = Some start_time;
      stage_end_time   = None;
    } in
    stage_stack := started_stage::(!stage_stack)

  let end_stage (stage_stack : stage_stack) : stage_description =
    let end_time = Sys.time () in
    let stage = List.hd (!stage_stack) in 
    stage_stack := List.tl (!stage_stack);
    let ended_stage = {
      stage_name       = stage.stage_name;
      stage_start_time = stage.stage_start_time;
      stage_end_time   = Some end_time;
    } in
    ended_stage

end

let current_stages : Stage.stage_stack = Stage.empty_stage_stack ()
let stage_condition () = (Settings.find Settings.verbose_level) > 1
let log_stage_new stage_name = 
  if stage_condition () then
    let stage = Stage.make_new_stage stage_name in
    Stage.start_stage current_stages stage;
    let str' = Printf.sprintf "| New Stage: \"%s\" |" stage_name in
    let n = (String.length str') - 2 in
    let line = "+" ^ (String.make n '-') ^ "+\n" in
    Output_helper.println out_channel (line ^ str' ^ "\n" ^ line);
    Output_helper.new_stage ()

let log_stage_end () = 
  if stage_condition () then
    let stage = Stage.end_stage current_stages in
    let stage_name     = Stage.stage_name     stage in
    let stage_duration = Stage.stage_duration stage in
    let str' = Printf.sprintf "| End Stage: \"%s\" [duration : %8.3f s] |" stage_name stage_duration in
    let n = (String.length str') - 2 in
    let line = "+" ^ (String.make n '-') ^ "+\n" in
    Output_helper.end_stage ();
    Output_helper.println out_channel (line ^ str' ^ "\n" ^ line)


(* setting logging *)
let log_settings () = if (Settings.find Settings.verbose_level) > 2 then log_normal ("Settings =\n" ^ (Settings.to_string ()))

let log_input_file_error filename str = log_error ("file \"" ^ filename ^ "\" => " ^ str)

let log_missing_data kind what where = log_panic ("the " ^ kind ^ " \"" ^ what ^ "\" is missing from the " ^ where)

let base_condition () = (Settings.find Settings.verbose_level) > 0
let log_execution str = if base_condition () then log_normal str
let log_data desc str = if (base_condition ()) && (Settings.find Settings.verbose_data) then log_normal (desc ^ (Lazy.force str))

let solver_condition () = (Settings.find Settings.verbose_level) > 1
let log_solver_execution str = if solver_condition () then log_normal str
let log_solver_data desc str =  if (solver_condition ()) && (Settings.find Settings.verbose_data) then (
  log_normal desc; log_normal (Lazy.force str))

let constraint_condition () = (Settings.find Settings.verbose_level) > 1
let log_constraint_execution str = if constraint_condition () then Output_helper.print out_channel str
let log_constraint_data desc str = if (constraint_condition ()) && (Settings.find Settings.verbose_data) then (log_normal desc; log_normal (Lazy.force str))



(*
(* loading logging *)

(* Not used *)
let log_setting_not_set str = if (Settings.find Settings.verbose_level) > 2 then Printf.printf "Warning: the %s is not set" str
let log_universe_full desc str = if Settings.get_bool_basic Settings.verbose_data_full then Output_helper.print out_channel (desc ^ (Lazy.force str))



(* translation into constraint logging *)

(* solver logging *)


*)
