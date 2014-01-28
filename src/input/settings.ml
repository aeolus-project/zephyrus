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

let string_of_string s = "\"" ^ s ^ "\""
let string_of_pair e1 e2 = "(" ^ e1 ^ ", " ^ e2 ^ ")"
let string_of_list l = "[" ^ (String.concat "; " l) ^ "]"

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

let convert_string        = get_ident
let string_domain_message = "any string"

let convert_int           = get_int
let int_domain_message    = "any integer"

(* 1.2. Zephyrus Execution mode. *) (* not used for now *)
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

(* 1.3. conversion of repository input *)
type repository = (string * string)
type repositories = repository list
let convert_repositories v      = List.map (fun e -> let (f,r) = get_pair e in (get_ident f, get_ident r)) (get_list v)
let string_of_repositories l    = string_of_list (List.map (fun (f, n) -> string_of_pair f n) l)
let repositories_domain_message = "a pair of a a file name containing a repository, with the repository's name"

(* 1.4. the different kind of optimization in zephyrus *)
type optim =
  | Optim_none
  | Optim_simple
  | Optim_compact
  | Optim_spread
  | Optim_conservative
let optim_assoc = [
  ("simple", Optim_simple);
  ("compact", Optim_compact);
  ("conservative", Optim_conservative);
  ("spread", Optim_spread);
  ("none", Optim_none) ]
let optim_assoc_revert = revert optim_assoc

let optim_names          = extract_names optim_assoc
let optim_map            = Data_common.String_map.of_assoc_list optim_assoc
let convert_optim v      = convert optim_map (get_ident v)
let string_of_optim v    = string_of_string (List.assoc v optim_assoc_revert)
let default_optim        = Optim_none
let optim_domain_message = String.concat " | " optim_names


(* 1.4. the different kind of solver available in zephyrus *)
type constraint_kind =
  | Constraint_classic
  | Constraint_linear
  | Bin_packing
let constraint_kind_assoc = [
  ("classic", Constraint_classic);
  ("linear", Constraint_linear);
  ("bin-packing", Bin_packing) ]
let constraint_kind_assoc_revert = revert constraint_kind_assoc

let constraint_kind_names          = extract_names constraint_kind_assoc
let constraint_kind_map            = Data_common.String_map.of_assoc_list constraint_kind_assoc
let convert_constraint_kind v      = convert constraint_kind_map (get_ident v)
let string_of_constraint_kind v    = string_of_string (List.assoc v constraint_kind_assoc_revert)
let default_constraint_kind        = Constraint_classic
let constraint_kind_domain_message = String.concat " | " constraint_kind_names

type solver =
  | Solver_none
  | Solver_gcode
  | Solver_g12
(*  | Solver_facile *)
  | Solver_custom
let solver_assoc = [
(*  ("facile", Solver_facile); *)
  ("none"  , Solver_none);
  ("g12"   , Solver_g12);
  ("gcode" , Solver_gcode);
  ("custom", Solver_custom) ]
let solver_assoc_revert = revert solver_assoc

let solver_names = extract_names solver_assoc
let solver_map = Data_common.String_map.of_assoc_list solver_assoc
let convert_solver v = convert solver_map (get_ident v)
let string_of_solver v = string_of_string (List.assoc v solver_assoc_revert)
let default_solver = Solver_gcode
let solver_domain_message = String.concat " | " solver_names

type solver_bin_packing = (* not used yet, lacking a bin packing solver ... *)
  | Solver_bin_packing_unknown
let solver_bin_packing_assoc =  []
let solver_bin_packing_assoc_revert = revert solver_bin_packing_assoc

let solver_bin_packing_names          = extract_names solver_bin_packing_assoc
let solver_bin_packing_map            = Data_common.String_map.of_assoc_list solver_bin_packing_assoc
let convert_solver_bin_packing v      = convert solver_bin_packing_map (get_ident v)
let string_of_solver_bin_packing v    = string_of_string (List.assoc v solver_bin_packing_assoc_revert)
let default_solver_bin_packing        = Solver_bin_packing_unknown
let solver_bin_packing_domain_message = String.concat " | " solver_bin_packing_names


(* 1.5. the different mean we have to generate the final configuration *)
type gen_bindings =
  | Gen_bindings_candy
  | Gen_bindings_constraint
let gen_bindings_assoc = [
  ("candy", Gen_bindings_candy);
  ("constraint", Gen_bindings_constraint) ]
let gen_bindings_assoc_revert = revert gen_bindings_assoc

let gen_bindings_names          = extract_names gen_bindings_assoc
let gen_bindings_map            = Data_common.String_map.of_assoc_list gen_bindings_assoc
let convert_gen_bindings v      = convert gen_bindings_map (get_ident v)
let string_of_gen_bindings v    = string_of_string (List.assoc v gen_bindings_assoc_revert)
let default_gen_bindings        = Gen_bindings_candy
let gen_bindings_domain_message = String.concat " | " gen_bindings_names


(* 1.6. the different mean we have to generate the packages we need to install *)
type gen_packages =
  | Gen_packages_none
  | Gen_packages_one
  | Gen_packages_all
let gen_packages_assoc = [
  ("none", Gen_packages_none); ("one", Gen_packages_one); ("all", Gen_packages_all)]
let gen_packages_assoc_revert = revert gen_packages_assoc

let gen_packages_names          = extract_names gen_packages_assoc
let gen_packages_map            = Data_common.String_map.of_assoc_list gen_packages_assoc
let convert_gen_packages v      = convert gen_packages_map (get_ident v)
let string_of_gen_packages v    = string_of_string (List.assoc v gen_packages_assoc_revert)
let default_gen_packages        = Gen_packages_one
let gen_packages_domain_message = String.concat " | " gen_packages_names


(* 1.7. the different kind of output files *)
type out_file_inner = 
  | Out_file_plain
  | Out_file_json
  | Out_file_json_v0
  | Out_file_json_v1
  | Out_file_graph_deployment
  | Out_file_graph_simplified
  | Out_file_graph_components
  | Out_file_graph_packages
  | Out_file_binpacking_problem
type out_file = (out_file_inner * string) 
type out_files = out_file list
let out_files_assoc = [
  ("plain"                      , Out_file_plain);
  ("json"                       , Out_file_json);
  ("json-v0"                    , Out_file_json_v0);
  ("json-v1"                    , Out_file_json_v1);
  ("simplified-deployment-graph", Out_file_graph_simplified);
  ("components-graph"           , Out_file_graph_components);
  ("packages-graph"             , Out_file_graph_packages);
  ("graph-deployment"           , Out_file_graph_deployment);
  ("binpacking-problem"         , Out_file_binpacking_problem); ]
let out_files_assoc_revert = revert out_files_assoc

let out_files_names = (extract_names out_files_assoc)
let out_files_map = Data_common.String_map.of_assoc_list out_files_assoc
let convert_out_files v = List.map (fun e -> let (k,f) = get_pair e in (convert out_files_map (get_ident k), get_ident f)) (get_list v)
let string_of_out_files v = string_of_list (List.map (fun (k,f) -> string_of_pair (string_of_string (List.assoc k out_files_assoc_revert)) (string_of_string f)) v)
let out_files_domain_message = string_of_pair "any string" (String.concat " | " out_files_names)

(* 1.8. benchmarks *)
type benchmark_choice = 
  | Benchmark_none
  | Benchmark_master_slave
  | Benchmark_wordpress
type benchmark_option  = (string * string) 
type benchmark_options = benchmark_option list
type benchmark = (benchmark_choice * benchmark_options)
let benchmark_choice_assoc = [
  ("none"        , Benchmark_none);
  ("master-slave", Benchmark_master_slave);
  ("wordpress"   , Benchmark_wordpress); ]
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
  | Bool of (value -> bool) * (bool -> string) * string
  | String of (value -> string) * (string -> string) * string
  | Int of (value -> int) * (int -> string) * string
  | Mode of (value -> mode) * (mode -> string) * string
  | Repositories of (value -> repositories) * (repositories -> string) * string
  | Optim of (value -> optim) * (optim -> string) * string
  | Constraint_kind of (value -> constraint_kind) * (constraint_kind -> string) * string
  | Solver of (value -> solver) * (solver -> string) * string
  | Solver_bin_packing of (value -> solver_bin_packing) * (solver_bin_packing -> string) * string
  | Gen_bindings of (value -> gen_bindings) * (gen_bindings -> string) * string
  | Gen_packages of (value -> gen_packages) * (gen_packages -> string) * string
  | Out_files of (value -> out_files) * (out_files -> string) * string
  | Benchmark of (value -> benchmark) * (benchmark -> string) * string

let bool_setting = Bool(convert_bool, string_of_bool, bool_domain_message)
let string_setting = String(convert_string, string_of_string, string_domain_message)
let int_setting = Int(convert_int, string_of_int, int_domain_message)
let mode_setting = Mode(convert_mode, string_of_mode, mode_domain_message)

let repositories_setting       = Repositories(convert_repositories, string_of_repositories, repositories_domain_message)
let optim_setting              = Optim(convert_optim, string_of_optim, optim_domain_message)
let constraint_kind_setting    = Constraint_kind(convert_constraint_kind, string_of_constraint_kind, constraint_kind_domain_message)
let solver_setting             = Solver(convert_solver, string_of_solver, solver_domain_message)
let solver_bin_packing_setting = Solver_bin_packing(convert_solver_bin_packing, string_of_solver_bin_packing, solver_bin_packing_domain_message)
let gen_bindings_setting       = Gen_bindings(convert_gen_bindings, string_of_gen_bindings, gen_bindings_domain_message)
let gen_packages_setting       = Gen_packages(convert_gen_packages, string_of_gen_packages, gen_packages_domain_message)
let out_files_setting          = Out_files(convert_out_files, string_of_out_files, out_files_domain_message)
let benchmark_setting          = Benchmark(convert_benchmark, string_of_benchmark, benchmark_domain_message)


(*/************************************************************************\*)
(*| 2. List of all settings with their kind                                |*)
(*\************************************************************************/*)


type 'a setting = string * setting_kind

(* 2.1. definition of all the settings. It can be long *)
let mode = ("mode", mode_setting)

    (* 2. Loading options *)
let input_file_universe = ("input-file-universe", string_setting)
let input_file_configuration = ("input-file-configuration", string_setting)
let input_file_specification = ("input-file-specification", string_setting)
let input_file_repositories = ("input-file-repositories", repositories_setting)
let input_optimization_function = ("input-optimization-function", optim_setting)
let import_universe = ("import-universe", bool_setting)
let import_repositories = ("import-repositories", bool_setting)
let import_initial_configuration = ("import-initial-configuration", bool_setting)
let import_specification = ("import-specification", bool_setting)
let import_optimization_function = ("import-optimization-function", bool_setting)

let append_repository_to_package_name = ("append-repository-to-package-name", bool_setting)
let eliminate_packages = ("eliminate-packages", bool_setting)
let modifiable_configuration = ("modifiable-configuration", bool_setting)
let flatten = ("flatten-the-model", bool_setting)

    (* 2. Checking the input *)
let check_universe = ("check-universe", bool_setting)
let check_repositories = ("check-repositories", bool_setting)
let check_initial_configuration = ("check-initial-configuration", bool_setting)
let check_universe_full = ("check-universe-full", bool_setting)
let check_specification = ("check-specification", bool_setting)

let check_settings = ("check-settings", bool_setting)

    (* 3. Pre_Processing *)
let detect_spec_is_empty = ("detect-spec-is-empty", bool_setting)
let component_type_trim = ("component-types-trim", bool_setting)
let detect_component_types_have_loop = ("detect-component-types-have-loop", bool_setting)
let detect_component_types_bounds = ("detect-component-types-bounds", bool_setting)
let package_trim = ("package-trim", bool_setting)

    (* 4. Solvers *)
let constraint_kind = ("constraint-kind", constraint_kind_setting)
let preprocess_solver = ("preprocess-solver", solver_setting)
let solver = ("solver", solver_setting)
let solver_bin_packing = ("solver-bin-packing", solver_bin_packing_setting)
let custom_solver_command = ("custom-solver-command", string_setting)
let custom_mzn2fzn_command = ("custom-mzn2fzn-command", string_setting)

    (* 5. Temporary Files *)
let preprocess_constraint_file = ("preprocess-constraint-file", string_setting)
let preprocess_solution_file = ("preprocess-solution-file", string_setting)
let preprocess_keep_constraint_file = ("preprocess-keep-constraint-file", bool_setting)
let preprocess_keep_solution_file = ("preprocess-keep-solution-file", bool_setting)

let solver_constraint_file = ("solver-constraint-file", string_setting)
let solver_solution_file = ("solver-solution-file", string_setting)
let solver_keep_constraint_file = ("solver-keep-constraint-file", bool_setting)
let solver_keep_solution_file = ("solver-keep-solution-file", bool_setting)

    (* 6. Configuration Generation *)
let generate_bindings = ("generate-bindings", gen_bindings_setting)
let generate_packages = ("generate-packages", gen_packages_setting)

    (* 7. Output Configuration *)
let results = ("results", out_files_setting)

    (* 8. Verbose Options *)
let verbose_level = ("verbose-level", int_setting)
let verbose_stage = ("verbose-stage", bool_setting)
let verbose_data = ("verbose-data", bool_setting)
let verbose_execution = ("verbose-execution", bool_setting)

    (* 9. Benchmark *)
let benchmark = ("benchmark", benchmark_setting)


(* 2.2. list of all settings *)
let all_settings = [
    mode;                                (* The Zephyrus functioning mode: {classic|validate|no-solving}. *)
    input_file_universe;                 (* The universe file. *)
    input_file_configuration;            (* The initial configuration file. *) (* The file where Zephyrus should look for the input configuration. *)
    input_file_specification;            (* The specification file. *)
    input_file_repositories;             (* The external repositories files. *)
    input_optimization_function;         (* The optimization function choice: {simple|compact|conservative|spread|none}*)
    import_universe;                     (* IMPLEMENTED BUT USELESS *) (* Should Zephyrus read the input universe file? *)
    import_repositories;                 (* IMPLEMENTED BUT USELESS *) (* Should Zephyrus read the input external repositories files? *)
    import_initial_configuration;        (* IMPLEMENTED BUT USELESS *) (* Should Zephyrus read the input initial configuration file? *)
    import_specification;                (* IMPLEMENTED BUT USELESS *) (* Should Zephyrus read the input specification file? *)
    import_optimization_function;        (* IMPLEMENTED BUT USELESS *) (* Should Zephyrus use the provided optimization function parameter? *)
    append_repository_to_package_name;   (* Prefix every package name with its repository name in the output. *)
    eliminate_packages;                  (* Eliminate the packages from solving, use component incompatibilities instead. *)
    check_universe;                      (* UNUSED *)
    check_repositories;                  (* UNUSED *)
    check_initial_configuration;         (* UNUSED *)
    check_universe_full;                 (* UNUSED *)
    check_specification;                 (* UNUSED *)
    check_settings;                      (* UNUSED *)
    detect_spec_is_empty;                (* UNUSED *)
    component_type_trim;                 (* UNUSED *)
    detect_component_types_have_loop;    (* UNUSED *)
    detect_component_types_bounds;       (* UNUSED *)
    package_trim;                        (* UNUSED *)
    constraint_kind;                     (* UNUSED *)
    preprocess_solver;                   (* DUNNO *) (* TODO: I think that for now we always use the main solver? Check it! *) (* Choose the solver used for preprocessing. *)
    solver;                              (* Choose the main constraint solver: {none|g12|gcode|custom} *)
    solver_bin_packing;                  (* UNUSED *)
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
    generate_bindings;                   (* UNUSED *)
    generate_packages;                   (* UNUSED *)
    results;                             (* Which forms of output should Zephyrus produce and to which files. *)
    verbose_level;                       (* How much information should Zephyrus print: 0,1,2,3 *)
    verbose_stage;                       (* UNUSED *)
    verbose_data;                        (* Should Zephyrus print the input data during execution. *)
    verbose_execution;                   (* UNUSED *)
    benchmark;                           (* Discard the normal input, synthetize a benchmark with given parameters instead. *)
    flatten                              (* Flatten the model (ignore the locations, packages and resources in the input) and use the Aeolus flat model approach to find the final configuration. *)
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

module Table = AddColumn(struct type t = bool let name = bool_setting let default = false end)(
            AddColumn(struct type t = string let name = string_setting let default = "" end)(
            AddColumn(struct type t = int let name = int_setting let default = -1 end)(
            AddListColumn(struct type el = repository let name = repositories_setting  end)(
            AddColumn(struct type t = optim let name = optim_setting let default = default_optim end)(
            AddColumn(struct type t = mode  let name = mode_setting  let default = default_mode  end)(
            AddColumn(struct type t = constraint_kind let name = constraint_kind_setting let default = default_constraint_kind end)(
            AddColumn(struct type t = solver let name = solver_setting let default = default_solver end)(
            AddColumn(struct type t = solver_bin_packing let name = solver_bin_packing_setting let default = default_solver_bin_packing end)(
            AddColumn(struct type t = gen_bindings let name = gen_bindings_setting let default = default_gen_bindings end)(
            AddColumn(struct type t = gen_packages let name = gen_packages_setting let default = default_gen_packages end)(
            AddListColumn(struct type el = out_file let name = out_files_setting end)(
            AddColumn(struct type t = benchmark let name = benchmark_setting let default = (Benchmark_none, []) end)(
              Empty(Base))))))))))))))

type t = Table.t
let table = Table.create 8
let add (s,k) b = match k with
  | Bool              (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | String            (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Int               (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Mode              (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Repositories      (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Optim             (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Constraint_kind   (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Solver            (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Solver_bin_packing(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Gen_bindings      (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Gen_packages      (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Out_files         (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Benchmark         (convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)

let find (s,k) = Table.find table k s
let mem (s,k) = Table.mem table s

let to_string () = let inner ((s,k) as key) res = if mem key then ("  " ^ s ^ (match k with
  | Bool              (convert, string_of, error_message) -> string_of (find key)
  | String            (convert, string_of, error_message) -> string_of (find key)
  | Int               (convert, string_of, error_message) -> string_of (find key)
  | Mode              (convert, string_of, error_message) -> string_of (find key)
  | Repositories      (convert, string_of, error_message) -> string_of (find key)
  | Optim             (convert, string_of, error_message) -> string_of (find key)
  | Constraint_kind   (convert, string_of, error_message) -> string_of (find key)
  | Solver            (convert, string_of, error_message) -> string_of (find key)
  | Solver_bin_packing(convert, string_of, error_message) -> string_of (find key)
  | Gen_bindings      (convert, string_of, error_message) -> string_of (find key)
  | Gen_packages      (convert, string_of, error_message) -> string_of (find key)
  | Out_files         (convert, string_of, error_message) -> string_of (find key)
  | Benchmark         (convert, string_of, error_message) -> string_of (find key)
  ))::res else res in
  String.concat "\n" (List.fold_right inner all_settings [])



(*/************************************************************************\*)
(*| 5. Helper functions                                                    |*)
(*\************************************************************************/*)


let add_string s v = add s (IdentValue v)
let add_double_lists s l1 l2  = add s (ListValue (List.map2 (fun n1 n2 -> PairValue(IdentValue(n1), IdentValue(n2))) l1 l2))
let add_benchmark b =
  let (benchmark_name, benchmark_options) = b in
  add benchmark (PairValue 
    (IdentValue (List.assoc benchmark_name benchmark_choice_assoc_revert), 
     ListValue (List.map (fun (option_key, option_value) -> 
       PairValue(IdentValue(option_key), IdentValue(option_value))
     ) benchmark_options)))

let enable_package_name_extension () = add append_repository_to_package_name (BoolValue true)
let enable_eliminate_packages     () = add eliminate_packages                (BoolValue true)

let get_input_file_universe () = if (find import_universe = true) & (mem input_file_universe) then Some(find input_file_universe) else None
let get_input_file_repositories () = if (find import_repositories = true) & (mem input_file_repositories) then Some(find input_file_repositories) else None
let get_input_file_initial_configuration () = if (find import_initial_configuration = true) & (mem input_file_configuration) then Some(find input_file_configuration) else None
let get_input_file_specification () = if (find import_specification = true) & (mem input_file_specification) then Some(find input_file_specification) else None
let get_input_optimization_function () = if (find import_optimization_function = true) & (mem input_optimization_function) then Some(find input_optimization_function) else None



let get_main_solver_file_extension () = ".mzn"
let get_main_input_file ()       = let res = find solver_constraint_file in if res = "" then "zephyrus-" ^ (get_main_solver_file_extension ()) else res
let get_main_output_file ()      = let res = find solver_solution_file in if res = "" then "zephyrus-.sol" else res
let get_main_file_informations () = ((get_main_input_file (), find solver_keep_constraint_file), (get_main_output_file (), find solver_keep_constraint_file))


let get_preprocess_solver_file_extension () = ".mzn"
let get_preprocess_input_file ()       = let res = find preprocess_constraint_file in if res = "" then "zephyrus-" ^ (get_preprocess_solver_file_extension ()) else res
let get_preprocess_output_file ()      = let res = find preprocess_solution_file in if res = "" then "zephyrus-.sol" else res

let get_preprocess_file_informations () = ((get_preprocess_input_file (), find preprocess_keep_constraint_file), (get_preprocess_output_file (), find preprocess_keep_constraint_file))

let get_custom_solver_command  () = if (find solver = Solver_custom) & (mem custom_solver_command) then Some(find custom_solver_command) else None
let get_custom_mzn2fzn_command () = if (mem custom_mzn2fzn_command) then Some(find custom_mzn2fzn_command) else None
