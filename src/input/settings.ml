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

(** Settings management. *)

(* Depends on
    - datatypes/Data_common
*)

open Data_common

let get_bool_basic r = match !r with | None -> false | Some(b) -> b


(*/************************************************************************\*)
(*| 1. Generic, Static value type, extraction function, and column functor |*)
(*\************************************************************************/*)

type value =
  | BoolValue  of bool
  | IntValue   of int
  | IdentValue of string
  | PairValue  of value * value
  | ListValue  of value list

let string_of_string s     = Printf.sprintf "\"%s\"" s
let string_of_pair fst snd = Printf.sprintf "(%s, %s)" fst snd
let string_of_list l       = Printf.sprintf "[%s]"(String.concat "; " l)

let rec string_of_value value = 
  match value with
  | BoolValue  b        -> string_of_bool b
  | IntValue   i        -> string_of_int i
  | IdentValue s        -> string_of_string s
  | PairValue  (v1, v2) -> string_of_pair (string_of_value v1) (string_of_value v2)
  | ListValue  l        -> string_of_list (List.map string_of_value l)

exception Wrong_value 

let get_bool  v = match v with | BoolValue(b)     -> b       | _ -> raise Wrong_value
let get_int   v = match v with | IntValue(i)      -> i       | _ -> raise Wrong_value
let get_ident v = match v with | IdentValue(s)    -> s       | _ -> raise Wrong_value
let get_pair  v = match v with | PairValue(v1,v2) -> (v1,v2) | _ -> raise Wrong_value
let get_list  v = match v with | ListValue(l)     -> l       | _ -> raise Wrong_value



module Settings_log = struct
  let out_channel = stdout
  let log_wrong_setting k = Output_helper.print out_channel ("Error in settings: unexpected key \"" ^ k ^ "\". Skipping its definition\n")
  let log setting value error_message = Output_helper.print out_channel (
      "Error in settings: unexpected value \"" ^ (string_of_value value) ^ " found in the definition of the setting \"" ^ setting ^ "\" "
    ^ "(valid values :" ^ error_message ^ "]). Skipping its definition\n")
end

let revert : ('a * 'b) list -> ('b * 'a) list = fun l -> List.fold_left (fun res (a,b) -> (b,a)::res) [] l

let extract_names l = fst (List.split l)
let convert map value = try Data_common.String_map.find value map with Not_found -> raise Wrong_value


(*/************************************************************************\*)
(*| 1. Conversion, string_of and error message for all kind of settings    |*)
(*\************************************************************************/*)

(* 1.1. Basic settings *)

let bool_names            = ["false"; "n"; "true"; "y"]
let convert_bool          = get_bool
let bool_domain_message   = String.concat " | " bool_names

let bool_value_of_bool b = BoolValue b

let convert_string        = get_ident
let string_domain_message = "any string"

let convert_int           = get_int
let int_domain_message    = "any integer"

(* 1.2. Execution mode. *)
type mode = 
  | Mode_classic
  | Mode_validate_initial_config
  | Mode_no_solving

let mode_assoc = [
  ("classic",     Mode_classic);
  ("validate",    Mode_validate_initial_config);
  ("no-solving",  Mode_no_solving) ]

let mode_assoc_revert = revert mode_assoc

let mode_names          = extract_names mode_assoc
let mode_map            = Data_common.String_map.of_assoc_list mode_assoc
let convert_mode v      = convert mode_map (get_ident v)
let string_of_mode v    = string_of_string (List.assoc v mode_assoc_revert)
let default_mode        = Mode_classic
let mode_domain_message = String.concat " | " mode_names

(* 1.3. External repositories to import *)
type repository = (string * string)
type repositories = repository list
let convert_repositories v      = List.map (fun e -> let (f,r) = get_pair e in (get_ident f, get_ident r)) (get_list v)
let string_of_repositories l    = string_of_list (List.map (fun (f, n) -> string_of_pair f n) l)
let repositories_domain_message = "a pair of a a file name containing a repository, with the repository's name"

(* 1.4. Optimization functions *)
type optimization_function =
  | Optimization_function_none
  | Optimization_function_simple
  | Optimization_function_compact
  | Optimization_function_spread
  | Optimization_function_conservative

let optimization_function_assoc = [
  ("simple"       , Optimization_function_simple);
  ("compact"      , Optimization_function_compact);
  ("conservative" , Optimization_function_conservative);
  ("spread"       , Optimization_function_spread);
  ("none"         , Optimization_function_none) ]

let optimization_function_assoc_revert = revert optimization_function_assoc

let optimization_function_names          = extract_names optimization_function_assoc
let optimization_function_map            = Data_common.String_map.of_assoc_list optimization_function_assoc
let convert_optimization_function v      = convert optimization_function_map (get_ident v)
let string_of_optimization_function v    = string_of_string (List.assoc v optimization_function_assoc_revert)
let default_optimization_function        = Optimization_function_none
let optimization_function_domain_message = String.concat " | " optimization_function_names


(* 1.4. Solvers *)
type solver =
(*  | Solver_facile *)
  | Solver_none
  | Solver_gecode
  | Solver_g12
  | Solver_g12_cpx
  | Solver_portfolio
  | Solver_custom

let solver_assoc = [
(*  ("facile", Solver_facile); *)
  ("none"     , Solver_none);
  ("gecode"   , Solver_gecode);
  ("g12"      , Solver_g12);
  ("g12_cpx"  , Solver_g12_cpx);
  ("portfolio", Solver_portfolio);
  ("custom"   , Solver_custom) ]

let solver_assoc_revert = revert solver_assoc

let solver_names = extract_names solver_assoc
let solver_map = Data_common.String_map.of_assoc_list solver_assoc
let convert_solver v = convert solver_map (get_ident v)
let string_of_solver v = string_of_string (List.assoc v solver_assoc_revert)
let default_solver = Solver_gecode
let solver_domain_message = String.concat " | " solver_names


(* 1.5. Final configuration generation: bindings *)
type generate_bindings =
  | Generate_bindings_candy
  | Generate_bindings_constraint

let generate_bindings_assoc = [
  ("candy"      , Generate_bindings_candy);
  ("constraint" , Generate_bindings_constraint) ]

let generate_bindings_assoc_revert = revert generate_bindings_assoc

let generate_bindings_names          = extract_names generate_bindings_assoc
let generate_bindings_map            = Data_common.String_map.of_assoc_list generate_bindings_assoc
let convert_generate_bindings v      = convert generate_bindings_map (get_ident v)
let string_of_generate_bindings v    = string_of_string (List.assoc v generate_bindings_assoc_revert)
let default_generate_bindings        = Generate_bindings_candy
let generate_bindings_domain_message = String.concat " | " generate_bindings_names


(* 1.6. Final configuration generation: packages *)
type generate_packages =
  | Generate_packages_none
  | Generate_packages_one
  | Generate_packages_all

let generate_packages_assoc = [
  ("none" , Generate_packages_none);
  ("one"  , Generate_packages_one);
  ("all"  , Generate_packages_all)]

let generate_packages_assoc_revert = revert generate_packages_assoc

let generate_packages_names          = extract_names generate_packages_assoc
let generate_packages_map            = Data_common.String_map.of_assoc_list generate_packages_assoc
let convert_generate_packages v      = convert generate_packages_map (get_ident v)
let string_of_generate_packages v    = string_of_string (List.assoc v generate_packages_assoc_revert)
let default_generate_packages        = Generate_packages_one
let generate_packages_domain_message = String.concat " | " generate_packages_names


(* 1.7. Output files *)
type output_type = 
  | Output_file_plain
  | Output_file_json
  | Output_file_json_v0
  | Output_file_json_v1
  | Output_file_graph_deployment
  | Output_file_graph_simplified
  | Output_file_graph_components
  | Output_file_graph_packages
  | Output_file_binpacking_problem
  | Output_file_statistics

type output_file = (output_type * string) 
type output_files = output_file list

let output_files_assoc = [
  ("plain"                      , Output_file_plain);
  ("json"                       , Output_file_json);
  ("json-v0"                    , Output_file_json_v0);
  ("json-v1"                    , Output_file_json_v1);
  ("simplified-deployment-graph", Output_file_graph_simplified);
  ("components-graph"           , Output_file_graph_components);
  ("packages-graph"             , Output_file_graph_packages);
  ("graph-deployment"           , Output_file_graph_deployment);
  ("binpacking-problem"         , Output_file_binpacking_problem);
  ("statistics"                 , Output_file_statistics); ]

let output_files_assoc_revert = revert output_files_assoc

let output_files_names = (extract_names output_files_assoc)
let output_files_map = Data_common.String_map.of_assoc_list output_files_assoc
let convert_output_files v = List.map (fun e -> let (k,f) = get_pair e in (convert output_files_map (get_ident k), get_ident f)) (get_list v)
let string_of_output_files v = string_of_list (List.map (fun (k,f) -> string_of_pair (string_of_string (List.assoc k output_files_assoc_revert)) (string_of_string f)) v)
let output_files_domain_message = string_of_pair "any string" (String.concat " | " output_files_names)

(* 1.8. Benchmarks *)
type benchmark_choice = 
  | Benchmark_none
  | Benchmark_master_slave
  | Benchmark_wordpress
  | Benchmark_wordpress_distributed

type benchmark_option  = (string * string) 
type benchmark_options = benchmark_option list
type benchmark = (benchmark_choice * benchmark_options)

let benchmark_choice_assoc = [
  ("none"                  , Benchmark_none);
  ("master-slave"          , Benchmark_master_slave);
  ("wordpress"             , Benchmark_wordpress);
  ("wordpress-distributed" , Benchmark_wordpress_distributed) ]

let benchmark_choice_assoc_revert = revert benchmark_choice_assoc

let benchmark_choice_names = (extract_names benchmark_choice_assoc)
let benchmark_choice_map = Data_common.String_map.of_assoc_list benchmark_choice_assoc
let convert_benchmark v = 
  let benchmark_choice_v, bechmark_options_v = get_pair v in
  let benchmark_choice = get_ident benchmark_choice_v in
  let benchmark_options = List.map (fun benchmark_option_v -> 
    let option_key_v, option_value_v = get_pair benchmark_option_v in
    (get_ident option_key_v, get_ident option_value_v)
  ) (get_list bechmark_options_v) in
  (convert benchmark_choice_map benchmark_choice, benchmark_options)

let string_of_benchmark v = 
  let benchmark_choice, benchmark_options = v in
  string_of_pair
    (string_of_string (List.assoc benchmark_choice benchmark_choice_assoc_revert))
    (string_of_list 
      (List.map (fun benchmark_option -> 
        let option_key, option_value = benchmark_option in
        string_of_pair
          (string_of_string option_key)
          (string_of_string option_value)
      ) benchmark_options))

let benchmark_domain_message = string_of_pair (String.concat " | " benchmark_choice_names) "list of options: key-value pairs"



(* 1.8. finally filling the setting_kind structure *)

type setting_kind = (* store the conversion and the string_of functions, and the error message *)
  | Bool                   of (value -> bool)                  * (bool                  -> string) * string
  | String                 of (value -> string)                * (string                -> string) * string
  | Int                    of (value -> int)                   * (int                   -> string) * string
  | Mode                   of (value -> mode)                  * (mode                  -> string) * string
  | Repositories           of (value -> repositories)          * (repositories          -> string) * string
  | Optimization_function  of (value -> optimization_function) * (optimization_function -> string) * string
  | Solver                 of (value -> solver)                * (solver                -> string) * string
  | Generate_bindings      of (value -> generate_bindings)     * (generate_bindings     -> string) * string
  | Generate_packages      of (value -> generate_packages)     * (generate_packages     -> string) * string
  | Output_files           of (value -> output_files)          * (output_files          -> string) * string
  | Benchmark              of (value -> benchmark)             * (benchmark             -> string) * string

let bool_setting   = Bool   (convert_bool,   string_of_bool,   bool_domain_message)
let string_setting = String (convert_string, string_of_string, string_domain_message)
let int_setting    = Int    (convert_int,    string_of_int,    int_domain_message)
let mode_setting   = Mode   (convert_mode,   string_of_mode,   mode_domain_message)

let repositories_setting           = Repositories          (convert_repositories,          string_of_repositories,          repositories_domain_message)
let optimization_function_setting  = Optimization_function (convert_optimization_function, string_of_optimization_function, optimization_function_domain_message)
let solver_setting                 = Solver                (convert_solver,                string_of_solver,                solver_domain_message)
let generate_bindings_setting      = Generate_bindings     (convert_generate_bindings,     string_of_generate_bindings,     generate_bindings_domain_message)
let generate_packages_setting      = Generate_packages     (convert_generate_packages,     string_of_generate_packages,     generate_packages_domain_message)
let output_files_setting           = Output_files          (convert_output_files,          string_of_output_files,          output_files_domain_message)
let benchmark_setting              = Benchmark             (convert_benchmark,             string_of_benchmark,             benchmark_domain_message)


(*/************************************************************************\*)
(*| 2. List of all settings with their kind                                |*)
(*\************************************************************************/*)


type 'a setting = string * setting_kind

(* 2.1. definition of all the settings.*)

let mode = ("mode", mode_setting)

(* Loading options *)
let input_file_universe         = ("input-file-universe",         string_setting)
let input_file_configuration    = ("input-file-configuration",    string_setting)
let input_file_specification    = ("input-file-specification",    string_setting)
let input_file_repositories     = ("input-file-repositories",     repositories_setting)
let input_optimization_function = ("input-optimization-function", optimization_function_setting)

let append_repository_to_package_name = ("append-repository-to-package-name", bool_setting)
let eliminate_packages                = ("eliminate-packages",                bool_setting)
let no_location_trimming              = ("use-all-locations",                 bool_setting)
let ralfs_redundant_constraints       = ("ralfs-redundant-constraints",       bool_setting)
let modifiable_configuration          = ("modifiable-configuration",          bool_setting)
let flatten                           = ("flatten-the-model",                 bool_setting)
let stop_after_solving                = ("stop-after-solving",                bool_setting)

(* Pre-processing options *)
let detect_spec_is_empty             = ("detect-spec-is-empty",             bool_setting)
let component_type_trim              = ("component-types-trim",             bool_setting)
let detect_component_types_have_loop = ("detect-component-types-have-loop", bool_setting)
let detect_component_types_bounds    = ("detect-component-types-bounds",    bool_setting)
let package_trim                     = ("package-trim",                     bool_setting)

(* Solver options *)
let preprocess_solver      = ("preprocess-solver",      solver_setting)
let solver                 = ("solver",                 solver_setting)
let custom_solver_command  = ("custom-solver-command",  string_setting)
let custom_mzn2fzn_command = ("custom-mzn2fzn-command", string_setting)

(* Temporary files options *)
let preprocess_constraint_file      = ("preprocess-constraint-file",      string_setting)
let preprocess_solution_file        = ("preprocess-solution-file",        string_setting)
let preprocess_keep_constraint_file = ("preprocess-keep-constraint-file", bool_setting)
let preprocess_keep_solution_file   = ("preprocess-keep-solution-file",   bool_setting)

let solver_constraint_file      = ("solver-constraint-file",      string_setting)
let solver_solution_file        = ("solver-solution-file",        string_setting)
let solver_keep_constraint_file = ("solver-keep-constraint-file", bool_setting)
let solver_keep_solution_file   = ("solver-keep-solution-file",   bool_setting)

(* Configuration generation options *)
let generate_bindings = ("generate-bindings", generate_bindings_setting)
let generate_packages = ("generate-packages", generate_packages_setting)

(* Output options *)
let outputs = ("results", output_files_setting)

(* Debug printing options *)
let verbose_level     = ("verbose-level",     int_setting)
let verbose_stage     = ("verbose-stage",     bool_setting)
let verbose_data      = ("verbose-data",      bool_setting)
let verbose_execution = ("verbose-execution", bool_setting)

(* Benchmark *)
let benchmark = ("benchmark", benchmark_setting)


(* 2.2. list of all settings *)
let all_settings = [
    mode;                                (* The Zephyrus functioning mode: {classic|validate|no-solving}. *)
    input_file_universe;                 (* The universe file. *)
    input_file_configuration;            (* The initial configuration file. *) (* The file where Zephyrus should look for the input configuration. *)
    input_file_specification;            (* The specification file. *)
    input_file_repositories;             (* The external repositories files. *)
    input_optimization_function;         (* The optimization function choice: {simple|compact|conservative|spread|none}*)
    append_repository_to_package_name;   (* Prefix every package name with its repository name in the output. *)
    eliminate_packages;                  (* Eliminate the packages from solving, use component incompatibilities instead. *)
    no_location_trimming;                (* Do not try to reduce the number of locations during the preprocessing. *)
    ralfs_redundant_constraints;         (* Add Ralf's redundant constraints for the *)
    detect_spec_is_empty;                (* UNUSED *)
    component_type_trim;                 (* UNUSED *)
    detect_component_types_have_loop;    (* UNUSED *)
    detect_component_types_bounds;       (* UNUSED *)
    package_trim;                        (* UNUSED *)
    preprocess_solver;                   (* DUNNO *) (* TODO: I think that for now we always use the main solver? Check it! *) (* Choose the solver used for preprocessing. *)
    solver;                              (* Choose the main constraint solver: {none|g12|gecode|custom} *)
    custom_solver_command;               (* Defined a custom command used to launch the external FlatZinc solver. *)
    custom_mzn2fzn_command;              (* Defined a custom command used to launch the MiniZinc-to-FlatZinc converter. *)
    preprocess_constraint_file;          (* ? *)
    preprocess_solution_file;            (* ? *)
    preprocess_keep_constraint_file;     (* ? *)
    preprocess_keep_solution_file;       (* ? *)
    solver_constraint_file;              (* ? *)
    solver_solution_file;                (* ? *)
    solver_keep_constraint_file;         (* ? *)
    solver_keep_solution_file;           (* ? *)
    generate_bindings;                   (* UNUSED: Use which method to generate bindings in the final configuration (candy algorithm / solver)? *)
    generate_packages;                   (* UNUSED: How to generate packages in the final configuration (all / only root packages / none)? *)
    outputs;                             (* Which forms of output should Zephyrus produce and to which files. *)
    verbose_level;                       (* How much information should Zephyrus print: 0,1,2,3 *)
    verbose_stage;                       (* UNUSED *)
    verbose_data;                        (* Should Zephyrus print the input data during execution. *)
    verbose_execution;                   (* UNUSED *)
    benchmark;                           (* Discard the normal input, synthetize a benchmark with given parameters instead. *)
    flatten;                             (* Flatten the model (ignore the locations, packages and resources in the input) and use the Aeolus flat model approach to find the final configuration. *)
    stop_after_solving                   (* Do not generate the final configuration, exit directly after the solving phase is over. *)
  ]

let setting_of_string s = match List.filter (fun (n,_) -> n = s) all_settings with
  | [] -> raise Not_found
  | k::l -> k

(*/************************************************************************\*)
(*| 3. Database and functions                                              |*)
(*\************************************************************************/*)

open Data_common_database.Database.Table

type 'a column = setting_kind

module Base = struct type key = string let compare = String.compare type 'a column = setting_kind let default = [] let aggregate = (@) end

module type Input = sig type t val name : setting_kind val default : t end
module AddColumn(I : Input)(T : S with type key = string and type 'a column = setting_kind) =
   AddOptional(WithoutAggregate(WithDefaultValue(WithoutChecking(struct include Base include I end))(I)))(T)

module type List_input = sig type el val name : setting_kind end
module AddListColumn(I : List_input)(T : S with type key = string and type 'a column = setting_kind) =
   AddOptional(WithAggregate(WithDefaultValue(WithoutChecking(struct include Base include I type t = I.el list end))(Base))(Base))(T)

module Table = 
            AddColumn    (struct type t  = bool                  let name = bool_setting                  let default = false                         end)(
            AddColumn    (struct type t  = string                let name = string_setting                let default = ""                            end)(
            AddColumn    (struct type t  = int                   let name = int_setting                   let default = -1                            end)(
            AddListColumn(struct type el = repository            let name = repositories_setting                                                      end)(
            AddColumn    (struct type t  = optimization_function let name = optimization_function_setting let default = default_optimization_function end)(
            AddColumn    (struct type t  = mode                  let name = mode_setting                  let default = default_mode                  end)(
            AddColumn    (struct type t  = solver                let name = solver_setting                let default = default_solver                end)(
            AddColumn    (struct type t  = generate_bindings     let name = generate_bindings_setting     let default = default_generate_bindings     end)(
            AddColumn    (struct type t  = generate_packages     let name = generate_packages_setting     let default = default_generate_packages     end)(
            AddListColumn(struct type el = output_file           let name = output_files_setting                                                      end)(
            AddColumn    (struct type t  = benchmark             let name = benchmark_setting             let default = (Benchmark_none, [])          end)(
              Empty(Base))))))))))))

type t = Table.t
let table = Table.create 8

let add (s,k) b = 
  match k with
  | Bool                  (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | String                (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Int                   (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Mode                  (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Repositories          (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Optimization_function (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Solver                (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Generate_bindings     (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Generate_packages     (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Output_files          (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Benchmark             (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)

let find (s,k) = Table.find table k s
let mem (s,k) = Table.mem table s

let to_string () : string = 

  let lines =
    List.filter_map (fun ((setting_name, setting_kind) as key) ->
      if mem key 
      then 
        let setting_value =
          match setting_kind with
          | Bool                  (convert, string_of, error_message) -> string_of (find key)
          | String                (convert, string_of, error_message) -> string_of (find key)
          | Int                   (convert, string_of, error_message) -> string_of (find key)
          | Mode                  (convert, string_of, error_message) -> string_of (find key)
          | Repositories          (convert, string_of, error_message) -> string_of (find key)
          | Optimization_function (convert, string_of, error_message) -> string_of (find key)
          | Solver                (convert, string_of, error_message) -> string_of (find key)
          | Generate_bindings     (convert, string_of, error_message) -> string_of (find key)
          | Generate_packages     (convert, string_of, error_message) -> string_of (find key)
          | Output_files          (convert, string_of, error_message) -> string_of (find key)
          | Benchmark             (convert, string_of, error_message) -> string_of (find key)
        in
        Some (Printf.sprintf "  %s = |%s|" setting_name setting_value)
      else None
    ) all_settings in
  
  String.concat "\n" lines



(*/************************************************************************\*)
(*| 5. Helper functions                                                    |*)
(*\************************************************************************/*)

let handle_setting_declaration setting value = add setting value

let add_string s v = add s (IdentValue v)

let add_double_lists s l1 l2  = add s (ListValue (List.map2 (fun n1 n2 -> PairValue(IdentValue(n1), IdentValue(n2))) l1 l2))

let add_benchmark b =
  let (benchmark_name, benchmark_options) = b in
  add benchmark (PairValue 
    (IdentValue (List.assoc benchmark_name benchmark_choice_assoc_revert), 
     ListValue (List.map (fun (option_key, option_value) -> 
       PairValue(IdentValue(option_key), IdentValue(option_value))
     ) benchmark_options)))

let switch_bool_setting setting b = add setting (bool_value_of_bool b)

let enable_package_name_extension        () = add append_repository_to_package_name (BoolValue true)

let enable_eliminate_packages            () = add eliminate_packages                (BoolValue true)

let enable_no_location_trimming          () = add no_location_trimming              (BoolValue true)
let disable_no_location_trimming         () = add no_location_trimming              (BoolValue false)

let enable_ralfs_redundant_constraints   () = add ralfs_redundant_constraints       (BoolValue true)
let disable_ralfs_redundant_constraints  () = add ralfs_redundant_constraints       (BoolValue false)

let enable_stop_after_solving            () = add stop_after_solving                (BoolValue true)

(* WTF? Why did this suddenly change type? *)
let get_input_file_universe              () : string                 = find input_file_universe
let get_input_file_repositories          () : (string * string) list = find input_file_repositories
let get_input_file_initial_configuration () : string                 = find input_file_configuration
let get_input_file_specification         () : string                 = find input_file_specification
let get_input_optimization_function      () : optimization_function  = find input_optimization_function


let get_main_solver_file_extension () = ".mzn"
let get_main_input_file  ()      = let res = find solver_constraint_file in if res = "" then "zephyrus-" ^ (get_main_solver_file_extension ()) else res
let get_main_output_file ()      = let res = find solver_solution_file   in if res = "" then "zephyrus-.sol"                                   else res

let get_preprocess_solver_file_extension () = ".mzn"
let get_preprocess_input_file  () = let res = find preprocess_constraint_file in if res = "" then "zephyrus-" ^ (get_preprocess_solver_file_extension ()) else res
let get_preprocess_output_file () = let res = find preprocess_solution_file   in if res = "" then "zephyrus-.sol"                                         else res


let get_custom_solver_command  () = if (find solver = Solver_custom) && (mem custom_solver_command) then Some(find custom_solver_command)  else None
let get_custom_mzn2fzn_command () = if (mem custom_mzn2fzn_command)                                 then Some(find custom_mzn2fzn_command) else None
