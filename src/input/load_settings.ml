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
 - Sys (from ocaml standard library)
 - Lexing (from ocaml standard library)
 - input/Settings_parser
 - input/Settings_lexer
*)

(* Module used to incremetally build a pair (useful for handling tuple command line arguments):
   - first we create the pair (make)
   - then we can repeat a sequence of these four operations:
     1. set the first element of the pair
     2. set the second element of the pair
     3. optional: get the pair (any number of times we need) 
     4. reset the pair 
  If an operation is performet out of this sequence it causes an exception (or simply an assertion fails). *)
module IncrementalPair: sig
  type ('a, 'b) t
  
  (* [make ()] creates a new pair with both elements unset. *)
  val make : unit -> ('a, 'b) t
  
  (* [set_fst p v] sets the first element of the pair [p] to value [v]. (Requires: neither the first nor the second value are set yet.) *)
  val set_fst : ('a, 'b) t -> 'a -> unit
  
  (* [set_snd p v] sets the SECOND element of the pair [p] to value [v]. (Requires: the first value is already set, the second value is not set yet.) *)
  val set_snd : ('a, 'b) t -> 'b -> unit
  
  (* [get p] returns the pair [p]. (Requires: both the first value and the second value are already set.) *)
  val get     : ('a, 'b) t -> 'a * 'b
  
  (* [reset p] unsets both values of the pair [p]. (Requires: both the first value and the second value are already set.) *)
  val reset   : ('a, 'b) t -> unit

end = struct 

  type ('a, 'b)  t = ('a option ref * 'b option ref)

  let is_fst_set (p : ('a, 'b) t) = match !(fst p) with Some _ -> true | None -> false
  let is_snd_set (p : ('a, 'b) t) = match !(snd p) with Some _ -> true | None -> false

  let make () : ('a, 'b) t = (ref None, ref None)
  
  let set_fst (p : ('a, 'b) t) (v : 'a) : unit =
    assert (not (is_fst_set p)); (* The fst should NOT be set at this point. *)
    assert (not (is_snd_set p)); (* The snd should NOT be set at this point. *)
    (fst p) := Some v
  
  let set_snd (p : ('a, 'b) t) (v : 'b) : unit =
    assert (     is_fst_set p);  (* The fst should     be set at this point. *)
    assert (not (is_snd_set p)); (* The snd should NOT be set at this point. *)
    (snd p) := Some v

  let get (p : ('a, 'b) t) : ('a * 'b) = 
    assert (is_fst_set p); (* The fst should be set at this point. *)
    assert (is_snd_set p); (* The snd should be set at this point. *)
    match (!(fst p), !(snd p)) with
    | Some(fst), Some(snd) -> (fst, snd)
    | _ -> failwith "Impossible error during handling the IncrementalPair (assertions should have failed first)!"
  
  let reset (p : ('a, 'b) t) : unit =
    assert (is_fst_set p); (* The fst should be set at this point. *)
    assert (is_snd_set p); (* The snd should be set at this point. *)
    (fst p) := None;
    (snd p) := None

end

(** Helper functions. *)

(* 1. Settings files. *)

(* Load a settings file. *)
let load_settings_file file = 
  ignore (Input_helper.parse_standard Settings_parser.main Settings_lexer.token file)


(* Default settings file. *)
let default_settings_file_path = "default.settings"


(* 2. External repositories to import. *)
let current_repository_data : (string, string) IncrementalPair.t = IncrementalPair.make ()

let add_current_repository_to_settings () = 
  let repository_name, repository_file = IncrementalPair.get current_repository_data in
  Settings.add Settings.input_file_repositories (Settings.ListValue [Settings.PairValue (Settings.IdentValue(repository_name), Settings.IdentValue(repository_file))])


(* 3. Outputs. *)
let current_output_data : (string, string) IncrementalPair.t = IncrementalPair.make ()

let add_current_output_to_settings () = 
  let output_type, output_file = IncrementalPair.get current_output_data in
  Settings.add Settings.outputs (Settings.ListValue [Settings.PairValue (Settings.IdentValue(output_type), Settings.IdentValue(output_file))])


(* 4. Benchmarks. *)
let benchmark_choice        = ref None
let benchmark_option_keys   = ref []
let benchmark_option_values = ref []

let set_benchmark_settings () =
  match !benchmark_choice with
  | Some benchmark_choice -> 
      let benchmark_options = List.combine !benchmark_option_keys !benchmark_option_values in
      let benchmark : Settings.benchmark = (benchmark_choice, benchmark_options) in
      Settings.add_benchmark benchmark
  | None -> ()


(* Mechanism simplifying on/off switch options. *)

(* Available switch option strings. *)
let on_off_switch_symbols = ["on"; "off"]

(* Convert a on/off switch option string (i.e. "on" or "off") to a boolean value. *)
let bool_of_on_off_switch_symbol s = match s with
  | "on"  -> true
  | "off" -> false
  | _     -> failwith (Printf.sprintf "Unrecognised on/off switch option value \"%s\"!" s)

(* Set a given setting to true or false using the given on/off switch option string. *)
let switch_on_off_setting setting (s : string) =
  Settings.add setting (Settings.bool_value_of_bool (bool_of_on_off_switch_symbol s))

(* Create an Arg.Symbol on/off switch for a given setting. *)
let arg_on_off_switch setting =
  Arg.Symbol (on_off_switch_symbols, (switch_on_off_setting setting))


(** Specification of command line options using the Arg module. *)
let speclist = 
  Arg.align [

    (* Input arguments *)
    ("-settings", Arg.String (fun filename -> load_settings_file filename), " The settings file (you can use this option more than once, subsequent setting files will be loaded in the given order).");
    ("-u",        Arg.String (fun filename -> Settings.add_string Settings.input_file_universe      filename), " The universe input file.");
    ("-ic",       Arg.String (fun filename -> Settings.add_string Settings.input_file_configuration filename), " The initial configuration input file.");
    ("-spec",     Arg.String (fun filename -> Settings.add_string Settings.input_file_specification filename), " The specification input file.");
    ("-repo",     
      Arg.Tuple ([
        (* We assume, that these two subparts of a tuple (corresponding to the name and file of a single imported repository) will be always handled directly after the other. *)
        Arg.String (fun repository_name -> IncrementalPair.set_fst current_repository_data repository_name);
        Arg.String (fun repository_file -> IncrementalPair.set_snd current_repository_data repository_file; add_current_repository_to_settings (); IncrementalPair.reset current_repository_data)
      ]), " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories).");

    (* Benchmarks *)
    ("-benchmark", Arg.Symbol (Settings.benchmark_choice_names, (fun benchmark_choice_name -> benchmark_choice := Some (List.assoc benchmark_choice_name Settings.benchmark_choice_assoc))), " The benchmark choice.");
    ("-benchmark-option", 
      Arg.Tuple ([
        Arg.String (fun benchmark_option_key   -> benchmark_option_keys   := benchmark_option_key::!benchmark_option_keys);
        Arg.String (fun benchmark_option_value -> benchmark_option_values := benchmark_option_value::!benchmark_option_values)
      ]), " Specify a benchmark option: (key, value).");

    (* Solving options *)
    ("-prefix-repos", Arg.Unit   (Settings.enable_package_name_extension), " Prefix all package names in imported repositories by the repository name.");
    ("-mode",         Arg.Symbol (Settings.mode_names, Settings.add_string Settings.mode), " The functioning mode" (* ^ ": \n\"classic\" generates the final configuration normally, \n\"validate\" validates the initial one, \n\"no-solving\" uses the initial configuration directly as the final one" *) ^ ".");
    
    (* Preprocessing options *)
    ("-no-packages",                     arg_on_off_switch Settings.eliminate_packages,          " Eliminate the packages from solving, use component incompatibilities instead (default: off).");
    ("-use-all-locations",               arg_on_off_switch Settings.no_location_trimming,        " Do not try to reduce the number of locations during the preprocessing (default: off).");
    ("-use-ralfs-redundant-constraints", arg_on_off_switch Settings.ralfs_redundant_constraints, " Generate Ralf's redundant constraints (default: off)." );

    (* Optimization function argument, solver choice *)
    ("-opt",    Arg.Symbol (Settings.optimization_function_names, Settings.add_string Settings.input_optimization_function), " The optimization function.");
    ("-solver", Arg.Symbol (Settings.solver_names,                Settings.add_string Settings.solver),                      " The solver choice."); 
    ("-custom-solver-command",  Arg.String (fun custom_solver_command  -> Settings.add_string Settings.custom_solver_command custom_solver_command),   " The custom solver command" (* ^ "(example: \"flatzinc -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");
    ("-custom-fzn2mzn-command", Arg.String (fun custom_mzn2fzn_command -> Settings.add_string Settings.custom_mzn2fzn_command custom_mzn2fzn_command), " The custom mzn2fzn converter command" (* ^ "(example: \"mzn2fzn -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");

    (* Output arguments *)
    ("-out",
      Arg.Tuple ([
        Arg.Symbol ( Settings.output_files_names, (fun output_type -> IncrementalPair.set_fst current_output_data output_type));
        Arg.String (                               fun output_file -> IncrementalPair.set_snd current_output_data output_file; add_current_output_to_settings (); IncrementalPair.reset current_output_data)
      ]), " The final configuration output file and the output format (you can specify multiple output files with different formats).");

    (* Other *)
    ("-print-path",         Arg.Unit ( fun () -> Unix.system "echo $PATH"; ignore (exit 0) ), " Print the $PATH variable and exit.");
    ("-stop-after-solving", Arg.Unit (Settings.enable_stop_after_solving),                    " Do not generate the final configuration, exit directly after the solving phase is over (useful for benchmarking).");
  ]

(** Zephyrus main executable usage description. *)
let usage = 
    "usage: " ^ Sys.argv.(0)
  ^ " [-settings settings-file]"
  ^ " [-u universe-file]"
  ^ " [-ic initial-configuration-file]"
  ^ " [-spec specification-file]"
  ^ " [-repo repository-name packages-file]*" 
  ^ " [-opt optimization-function]"
  ^ " [-solver solver]"
  ^ " [-out output-format output-file]*"

open Settings

let load () = 

  (* 1. Load settings from the default settings file (if it exists). *)
  begin
    if Sys.file_exists default_settings_file_path
    then load_settings_file default_settings_file_path
  end;


  (* 2. Handle command line settings. *)

  (* Handle the command line arguments one by one. *)
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;

  (* Post-treatment of command line settings: *)
  set_benchmark_settings (); (* Benchmarks. *)


  (* 3. Print settings as they are now. *)
  Zephyrus_log.log_settings ()


let check_settings () = (* TODO : what to do? *) ()
