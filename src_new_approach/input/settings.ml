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

let extract_names l = List.fold_left (fun res (n,v) -> ("\"" ^ n ^ "\"")::res) [] l
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
  | Mode_flat
  | Mode_bin_packing
let mode_assoc = [
  ("classic", Mode_classic);
  ("flat", Mode_flat);
  ("bin-packing", Mode_bin_packing) ]
let mode_assoc_revert = revert mode_assoc

let mode_names          = extract_names mode_assoc
let mode_map            = Data_common.String_map.of_direct_list mode_assoc
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
let optim_map            = Data_common.String_map.of_direct_list optim_assoc
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
let constraint_kind_map            = Data_common.String_map.of_direct_list constraint_kind_assoc
let convert_constraint_kind v      = convert constraint_kind_map (get_ident v)
let string_of_constraint_kind v    = string_of_string (List.assoc v constraint_kind_assoc_revert)
let default_constraint_kind        = Constraint_classic
let constraint_kind_domain_message = String.concat " | " constraint_kind_names

type solver =
  | Solver_none
  | Solver_gcode
  | Solver_g12
  | Solver_facile
let solver_assoc = [
  ("facile", Solver_facile);
  ("g12"   , Solver_g12);
  ("gcode" , Solver_gcode);
  ("none"  , Solver_none) ]
let solver_assoc_revert = revert solver_assoc

let solver_names = extract_names solver_assoc
let solver_map = Data_common.String_map.of_direct_list solver_assoc
let convert_solver v = convert solver_map (get_ident v)
let string_of_solver v = string_of_string (List.assoc v solver_assoc_revert)
let default_solver = Solver_gcode
let solver_domain_message = String.concat " | " solver_names

type solver_bin_packing = (* not used yet, lacking a bin packing solver ... *)
  | Solver_bin_packing_unknown
let solver_bin_packing_assoc =  []
let solver_bin_packing_assoc_revert = revert solver_bin_packing_assoc

let solver_bin_packing_names          = extract_names solver_bin_packing_assoc
let solver_bin_packing_map            = Data_common.String_map.of_direct_list solver_bin_packing_assoc
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
let gen_bindings_map            = Data_common.String_map.of_direct_list gen_bindings_assoc
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
let gen_packages_map            = Data_common.String_map.of_direct_list gen_packages_assoc
let convert_gen_packages v      = convert gen_packages_map (get_ident v)
let string_of_gen_packages v    = string_of_string (List.assoc v gen_packages_assoc_revert)
let default_gen_packages        = Gen_packages_one
let gen_packages_domain_message = String.concat " | " gen_packages_names


(* 1.7. the different kind of output files *)
type out_file_inner = 
  | Out_file_plain
  | Out_file_json
  | Out_file_graph_deployment
  | Out_file_graph_simplified
  | Out_file_graph_components
  | Out_file_graph_packages
type out_file = (out_file_inner * string) 
type out_files = out_file list
let out_files_assoc = [
  ("plain"                      , Out_file_plain);
  ("json"                       , Out_file_json);
  ("simplified-deployment-graph", Out_file_graph_simplified);
  ("components-graph"           , Out_file_graph_components);
  ("packages-graph"             , Out_file_graph_packages);
  ("graph-deployment"           , Out_file_graph_deployment) ]
let out_files_assoc_revert = revert out_files_assoc

let out_files_names = (extract_names out_files_assoc)
let out_files_map = Data_common.String_map.of_direct_list out_files_assoc
let convert_out_files v = List.map (fun e -> let (k,f) = get_pair e in (convert out_files_map (get_ident k), get_ident f)) (get_list v)
let string_of_out_files v = string_of_list (List.map (fun (k,f) -> string_of_pair (string_of_string (List.assoc k out_files_assoc_revert)) (string_of_string f)) v)
let out_files_domain_message = string_of_pair "any string" (String.concat " | " out_files_names)


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


(* 2.2. list of all settings *)
let all_settings = [
    mode;
    input_file_universe;
    input_file_configuration;
    input_file_specification;
    input_file_repositories;
    input_optimization_function;
    import_universe;
    import_repositories;
    import_initial_configuration;
    import_specification;
    import_optimization_function;
    append_repository_to_package_name;
    check_universe;
    check_repositories;
    check_initial_configuration;
    check_universe_full;
    check_specification;
    check_settings;
    detect_spec_is_empty;
    component_type_trim;
    detect_component_types_have_loop;
    detect_component_types_bounds;
    package_trim;
    constraint_kind;
    preprocess_solver;
    solver;
    solver_bin_packing;
    preprocess_constraint_file;
    preprocess_solution_file;
    preprocess_keep_constraint_file;
    preprocess_keep_solution_file;
    solver_constraint_file;
    solver_solution_file;
    solver_keep_constraint_file;
    solver_keep_solution_file;
    generate_bindings;
    generate_packages;
    results;
    verbose_level;
    verbose_stage;
    verbose_data;
    verbose_execution
  ]

let setting_of_string s = match List.filter (fun (n,_) -> n = s) all_settings with
  | [] -> raise Not_found
  | k::l -> k

(*/************************************************************************\*)
(*| 3. Database and functions                                              |*)
(*\************************************************************************/*)

open Data_common.Database.Table

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
            AddColumn(struct type t = constraint_kind let name = constraint_kind_setting let default = default_constraint_kind end)(
            AddColumn(struct type t = solver let name = solver_setting let default = default_solver end)(
            AddColumn(struct type t = solver_bin_packing let name = solver_bin_packing_setting let default = default_solver_bin_packing end)(
            AddColumn(struct type t = gen_bindings let name = gen_bindings_setting let default = default_gen_bindings end)(
            AddColumn(struct type t = gen_packages let name = gen_packages_setting let default = default_gen_packages end)(
            AddListColumn(struct type el = out_file let name = out_files_setting end)(
              Empty(Base))))))))))))

type t = Table.t
let table = Table.create 8
let add (s,k) b = match k with
  | Bool(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | String(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Int(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Mode(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Repositories(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Optim(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Constraint_kind(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Solver(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Solver_bin_packing(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Gen_bindings(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Gen_packages(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)
  | Out_files(convert, string_of, error_message) -> (try Table.add table s; Table.add_to_column table k s (convert b) with Wrong_value -> Settings_log.log s b error_message)

let find (s,k) = Table.find table k s
let mem (s,k) = Table.mem table s

let to_string () = let inner ((s,k) as key) res = if mem key then ("  " ^ s ^ (match k with
  | Bool(convert, string_of, error_message) -> string_of (find key)
  | String(convert, string_of, error_message) -> string_of (find key)
  | Int(convert, string_of, error_message) -> string_of (find key)
  | Mode(convert, string_of, error_message) -> string_of (find key)
  | Repositories(convert, string_of, error_message) -> string_of (find key)
  | Optim(convert, string_of, error_message) -> string_of (find key)
  | Constraint_kind(convert, string_of, error_message) -> string_of (find key)
  | Solver(convert, string_of, error_message) -> string_of (find key)
  | Solver_bin_packing(convert, string_of, error_message) -> string_of (find key)
  | Gen_bindings(convert, string_of, error_message) -> string_of (find key)
  | Gen_packages(convert, string_of, error_message) -> string_of (find key)
  | Out_files(convert, string_of, error_message) -> string_of (find key)))::res else res in
  String.concat "\n" (List.fold_right inner all_settings [])



(*/************************************************************************\*)
(*| 5. Helper functions                                                    |*)
(*\************************************************************************/*)


let add_string s v = add s (IdentValue v)
let add_double_lists s l1 l2  = add s (ListValue (List.map2 (fun n1 n2 -> PairValue(IdentValue(n1), IdentValue(n2))) l1 l2))

let enable_package_name_extension () = add append_repository_to_package_name (BoolValue true)

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


(*/************************************************************************\*)
(*| 4. Previous version of the file                                        |*)
(*\************************************************************************/*)



(*




let convert setting list map value = try Some(Data_common.String_map.find value map) with
 | Not_found -> Settings_log.log setting value list; None



(* No Need For A Number: Essencial *)

let zephyrus_mode : mode option ref = ref None (* NotUsedYet *)

(* 01. Input Files *)

let input_file_universe              : string option ref          = ref None (* the main file of the universe *)
let input_file_repositories          : (string * string) list ref = ref []   (* the file for the repositories, with their names *)
let input_file_initial_configuration : string option ref          = ref None (* initial configuration *)
let input_file_specification         : string option ref          = ref None (* specification file *)
let input_optimization_function      : optim option ref           = ref None (* Optimization function *)

let input_file_settings              : string list ref          = ref []

let set_universe_input_file file = input_file_universe := Some(file)
let add_external_repository name file = input_file_repositories := (name, file)::!input_file_repositories
let set_input_configuration_file file = input_file_initial_configuration := Some(file)
let set_specification_file file = input_file_specification := Some(file)

let set_optimization_function n = input_optimization_function := (convert "optimization function" optim_names optim_map n)


let add_settings_file file = input_file_settings := file::!input_file_settings


(* TODO: Add options for the syntax of files ? *)


(* 02. Which initial Data to Generate *)

let data_generation_universe              : bool option ref = ref None
let data_generation_repositories          : bool option ref = ref None
let data_generation_initial_configuration : bool option ref = ref None
let data_generation_specification         : bool option ref = ref None
let data_generation_optimization_function : bool option ref = ref None

let get_input_file_universe () = if !data_generation_universe = Some(false) then None else !input_file_universe
let get_input_file_repositories () = if !data_generation_repositories = Some(false) then None else Some(!input_file_repositories)
let get_input_file_initial_configuration () = if !data_generation_initial_configuration = Some(false) then None else !input_file_initial_configuration
let get_input_file_specification () = if !data_generation_specification = Some(false) then None else !input_file_specification
let get_input_optimization_function () = if !data_generation_optimization_function = Some(false) then None else !input_optimization_function


let data_package_name_extended : bool option ref = ref None
let set_package_name_extended b = data_package_name_extended := Some(b)
let extend_package_name_with_repository () = set_package_name_extended true

let data_check_universe              : bool option ref = ref None (* NotUsedYet *)
let data_check_repositories          : bool option ref = ref None (* NotUsedYet *)
let data_check_initial_configuration : bool option ref = ref None (* NotUsedYet *)
let data_check_universe_full         : bool option ref = ref None (* NotUsedYet *)
let data_check_specification         : bool option ref = ref None (* NotUsedYet *)

let data_check_settings : bool option ref = ref None (* NotUsedYet *)


(* 03. Pre Processing *)

let pre_process_spec_wf_detection           : bool option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection        : bool option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection_solver : solver option ref = ref None (* NotUsedYet *)

let pre_process_universe_loop_detection     : bool option ref = ref None (* NotUsedYet *)
let pre_process_universe_bound_computation  : bool option ref = ref None (* NotUsedYet *)

let pre_process_package_coinst              : bool option ref = ref None (* NotUsedYet *)
let pre_process_package_trim_package        : bool option ref = ref None (* NotUsedYet *)
let pre_process_component_types_trim        : bool option ref = ref None (* NotUsedYet *)


(* 04. Constraint Solver *)

let constraint_weight_locations        : int option ref = ref None (* NotUsedYet *)
let constraint_weight_component_types  : int option ref = ref None (* NotUsedYet *)
let constraint_weight_packages         : int option ref = ref None (* NotUsedYet *)

let constraint_solver_classic_linear   : bool option ref = ref None (* NotUsedYet *)
let constraint_preprocess_solver       : solver option ref = ref None (* NotUsedYet *)
let constraint_main_solver             : solver option ref = ref None (* NotUsedYet *)

let set_constraint_main_solver n = constraint_main_solver := (convert "main constraint solver" solver_names solver_map n)
let get_constraint_main_solver () = match !constraint_main_solver with
  | None    -> Solver_g12
  | Some(s) -> s
let get_main_solver_file_extension () = ".mzn"

let set_constraint_preprocess_solver n = constraint_preprocess_solver := (convert "preprocess constraint solver" solver_names solver_map n)
let get_constraint_preprocess_solver () = match !constraint_preprocess_solver with
  | None    -> Solver_g12
  | Some(s) -> s
let get_preprocess_solver_file_extension () = ".mzn"


let constraint_solver_bin_packing_kind : solver_bin_packing option ref = ref None (* NotUsedYet *)


(* 05. Temporary Files *)

let pre_process_input_file       : string option ref = ref None (* NotUsedYet *)
let pre_process_output_file      : string option ref = ref None (* NotUsedYet *)
let pre_process_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let pre_process_output_file_keep : bool option ref = ref None (* NotUsedYet *)

let get_preprocess_input_file ()       = match !pre_process_input_file with | None -> "zephyrus-" ^ (get_preprocess_solver_file_extension ()) | Some(file) -> file
let get_preprocess_output_file ()      = match !pre_process_output_file with | None -> "zephyrus-.sol" | Some(file) -> file
let get_keep_preprocess_input_file ()  = get_bool_basic pre_process_input_file_keep
let get_keep_preprocess_output_file () = get_bool_basic pre_process_output_file_keep

let get_preprocess_file_informations () =
  ((get_preprocess_input_file (), get_keep_preprocess_input_file ()), (get_preprocess_output_file (), get_keep_preprocess_output_file ()))

let constraint_solver_flat_input_file       : string option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_output_file      : string option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_output_file_keep : bool option ref = ref None (* NotUsedYet *)

let constraint_solver_classic_input_file       : string option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_output_file      : string option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_output_file_keep : bool option ref = ref None (* NotUsedYet *)

let get_main_input_file ()       = match !constraint_solver_classic_input_file with | None -> "zephyrus-" ^ (get_main_solver_file_extension ()) | Some(file) -> file
let get_main_output_file ()      = match !constraint_solver_classic_output_file with | None -> "zephyrus-.sol" | Some(file) -> file
let get_keep_main_input_file ()  = get_bool_basic constraint_solver_classic_input_file_keep
let get_keep_main_output_file () = get_bool_basic constraint_solver_classic_output_file_keep

let get_main_file_informations () =
  ((get_main_input_file (), get_keep_main_input_file ()), (get_main_output_file (), get_keep_main_output_file ()))


(* 06. Configuration Generation *)

let configuration_generation_bindings : gen_bindings option ref = ref None (* NotUsedYet *)
let configuration_generation_packages : gen_packages option ref = ref None (* NotUsedYet *)


(* 07. Output Configuration *)

let output_file                  : out_files ref = ref [] (* NotUsedYet *)
let add_output_file kind file = let kind' = (convert "output file kind" out_files_names out_files_map kind) in match kind' with
 | Some(kind'') -> output_file := (kind'', file)::!output_file
 | None -> ()
let get_output_files () = !output_file

(* 08. Verbose Options *)

let verbose_stage : bool option ref = ref None
let verbose_activities : bool option ref = ref None

(* settings *)

let verbose_settings_check_warning : bool option ref = ref None
let verbose_settings_check_error   : bool option ref = ref None
let verbose_settings_non_set       : bool option ref = ref None
let verbose_settings_data          : bool option ref = ref None

(* 08.1. inputs *)

let verbose_input_warning : bool option ref = ref None
let verbose_input_error   : bool option ref = ref None

let verbose_input_universe              : bool option ref = ref None
let verbose_input_repositories          : bool option ref = ref None
let verbose_input_universe_full         : bool option ref = ref None
let verbose_input_initial_configuration : bool option ref = ref None
let verbose_input_specification         : bool option ref = ref None
let verbose_input_optimization_function : bool option ref = ref None

let verbose_data              : bool option ref = ref None
let verbose_data_full         : bool option ref = ref None


let verbose_input_universe_check              : bool option ref = ref None
let verbose_input_repositories_check          : bool option ref = ref None
let verbose_input_initial_configuration_check : bool option ref = ref None
let verbose_input_specification_check         : bool option ref = ref None

(* 08.2. pre-process *)

let verbose_spec_fw_detection                        : bool option ref = ref None
let verbose_spec_empty_detection                     : bool option ref = ref None
let verbose_spec_fw_detection_activities             : bool option ref = ref None
let verbose_spec_empty_detection_activities          : bool option ref = ref None

let verbose_universe_graph                           : bool option ref = ref None
let verbose_universe_loop_detection                  : bool option ref = ref None
let verbose_universe_bound_detection                 : bool option ref = ref None
let verbose_universe_loop_detection_activities       : bool option ref = ref None
let verbose_universe_bound_detection_activities      : bool option ref = ref None

let verbose_universe_package_trim_package            : bool option ref = ref None
let verbose_universe_component_types_trim            : bool option ref = ref None
let verbose_universe_package_trim_package_activities : bool option ref = ref None
let verbose_universe_component_types_trim_activities : bool option ref = ref None

(* 08.3. constraints *)

let verbose_constraint                   : bool option ref = ref None
let verbose_constraint_solver_activities : bool option ref = ref None
let verbose_solution                     : bool option ref = ref None

(* 08.4. configuration generation *)

let verbose_gen_configuration_init    : bool option ref = ref None
let verbose_gen_configuration_package : bool option ref = ref None
let verbose_gen_configuration_full    : bool option ref = ref None



(* 09. Post Processing *)

let generate_plan : bool option ref = ref None (* for instance. TODO: must be expanded when we know more. *)




(* 10. Very Simple functions *)


let get_bool_basic r = match !r with | None -> false | Some(b) -> b
let get_weight w = match !w with | None -> 1 | Some(i) -> i




(* PRINTING *)

type setting =
  | StringSetting               of string option
  | BoolSetting                 of bool option
  | IntSetting                  of int option
  | FileRepositoriesSetting     of (string * string) list
  | ModeSetting                 of mode option
  | OptimizationFunctionSetting of optim option
  | SolverKindSetting           of solver option
  | SolverBinPackingKindSetting of solver_bin_packing option
  | ConfGenBindingsSetting      of gen_bindings option
  | ConfGenPackagesSetting      of gen_packages option
  | OutputFileSetting           of out_files

let string_of_mode = function
  | Mode_classic     -> "classic"
  | Mode_flat        -> "flat"
  | Mode_bin_packing -> "bin_packing"

let string_of_optim = function     
  | Optim_none         -> "none"
  | Optim_simple       -> "simple"
  | Optim_compact      -> "compact"
  | Optim_spread       -> "spread"
  | Optim_conservative -> "conservative"

let string_of_solver = function
  | Solver_none   -> "none"
  | Solver_gcode  -> "gcode"
  | Solver_g12    -> "g12"
  | Solver_facile -> "facile"

let string_of_solver_bin_packing = function
  | Solver_bin_packing_unknown -> "Solver_bin_packing_unknown"

let string_of_conf_gen_bindings = function
  | Gen_bindings_candy      -> "Gen_bindings_candy"
  | Gen_bindings_constraint -> "Gen_bindings_constraint"

let string_of_conf_gen_packages = function
  | Gen_packages_none -> "Gen_packages_none"
  | Gen_packages_one  -> "Gen_packages_one"
  | Gen_packages_all  -> "Gen_packages_all"

let string_of_out_file = function
  | Out_file_plain            -> "Out_file_plain"
  | Out_file_json             -> "Out_file_json"
  | Out_file_graph_deployment -> "Out_file_graph_deployment"
  | Out_file_graph_simplified -> "Out_file_graph_simplified"
  | Out_file_graph_components -> "Out_file_graph_components"
  | Out_file_graph_packages   -> "Out_file_graph_packages"

let string_of_file_repositories (file_repositories : (string * string) list) =
  let repositories_strings =
    List.map (fun (repository_name, file) ->
      Printf.sprintf "%s : \"%s\"" repository_name file
    ) file_repositories
  in
  Printf.sprintf "[%s]" (String.concat ", " repositories_strings)

let string_of_out_files (out_files : out_files) =
  let out_files_strings =
    List.map (fun (out_file, file) ->
      Printf.sprintf "%s : \"%s\"" (string_of_out_file out_file) file
    ) out_files
  in
  Printf.sprintf "[%s]" (String.concat ", " out_files_strings)

let string_of_setting = 
  let none_or x string_of_x = 
    match x with
    | None   -> "[None]"
    | Some x -> string_of_x x
  in
  function
  | StringSetting               x -> none_or x (fun s -> Printf.sprintf "\"%s\"" s)
  | BoolSetting                 x -> none_or x (fun b -> if b then "true" else "false")
  | IntSetting                  x -> none_or x (fun i -> Printf.sprintf "\"%d\"" i)
  | FileRepositoriesSetting     l -> string_of_file_repositories l
  | ModeSetting                 x -> none_or x string_of_mode
  | OptimizationFunctionSetting x -> none_or x string_of_optim
  | SolverKindSetting           x -> none_or x string_of_solver
  | SolverBinPackingKindSetting x -> none_or x string_of_solver_bin_packing
  | ConfGenBindingsSetting      x -> none_or x string_of_conf_gen_bindings
  | ConfGenPackagesSetting      x -> none_or x string_of_conf_gen_packages
  | OutputFileSetting           l -> string_of_out_files l

let settings_printing_functions : (string * (unit -> setting)) list = [
  ("zephyrus-mode"                , fun () -> ModeSetting(!zephyrus_mode));

(* 01. Input Files *)
  ("input-file-universe"          , fun () -> StringSetting(!input_file_universe)                      );
  ("input-file-configuration"     , fun () -> StringSetting(!input_file_initial_configuration)         );
  ("input-file-specification"     , fun () -> StringSetting(!input_file_specification)                 );
  ("input-file-repositories"      , fun () -> FileRepositoriesSetting(!input_file_repositories)        );
  ("input-optimization-function"  , fun () -> OptimizationFunctionSetting(!input_optimization_function));

(* 02. Which initial Data to Generate *)
  ("import-universe"              , fun () -> BoolSetting(!data_generation_universe)             );
  ("import-repositories"          , fun () -> BoolSetting(!data_generation_repositories)         );
  ("import-initial-configuration" , fun () -> BoolSetting(!data_generation_initial_configuration));
  ("import-specification"         , fun () -> BoolSetting(!data_generation_specification)        );
  ("import-optimization-function" , fun () -> BoolSetting(!data_generation_optimization_function));

  ("check-universe"               , fun () -> BoolSetting(!data_check_universe)             );
  ("check-repositories"           , fun () -> BoolSetting(!data_check_repositories)         );
  ("check-initial-configuration"  , fun () -> BoolSetting(!data_check_initial_configuration));
  ("check-universe_full"          , fun () -> BoolSetting(!data_check_universe_full)        );
  ("check-specification"          , fun () -> BoolSetting(!data_check_specification)        );

  ("check-settings"               , fun () -> BoolSetting(!data_check_settings));

(* 03. Pre Processing *)

  ( "detect-spec-well-formedness"      , fun () -> BoolSetting(!pre_process_spec_wf_detection)                );
  ( "detect-spec-is-empty"             , fun () -> BoolSetting(!pre_process_spec_empty_detection)             );
  ( "detect-spec-solver"               , fun () -> SolverKindSetting(!pre_process_spec_empty_detection_solver));

  ( "detect-component-types-have-loop" , fun () -> BoolSetting(!pre_process_universe_loop_detection)   );
  ( "detect-component-types-bounds"    , fun () -> BoolSetting(!pre_process_universe_bound_computation));

  ( "package-coinst"                   , fun () -> BoolSetting(!pre_process_package_coinst)      );
  ( "package-trim"                     , fun () -> BoolSetting(!pre_process_package_trim_package));
  ( "component-types-trim"             , fun () -> BoolSetting(!pre_process_component_types_trim));


(* 04. Constraint Solver *)
  ( "weight-locations"             , fun () -> IntSetting(!constraint_weight_locations)      );
  ( "weight-component-types"       , fun () -> IntSetting(!constraint_weight_component_types));
  ( "weight-packages"              , fun () -> IntSetting(!constraint_weight_packages)       );

  ( "solver-use-linear-constraint" , fun () -> BoolSetting                (!constraint_solver_classic_linear)  );
  ( "solver"                       , fun () -> SolverKindSetting          (!constraint_main_solver)    );
  ( "solver-bin-packing"           , fun () -> SolverBinPackingKindSetting(!constraint_solver_bin_packing_kind));


(* 05. Temporary Files *)

  ( "detect-spec-is-empty-constraint-file"      , fun () -> StringSetting(!pre_process_input_file)    );
  ( "detect-spec-is-empty-solution-file"        , fun () -> StringSetting(!pre_process_output_file)   );
  ( "detect-spec-is-empty-keep-constraint-file" , fun () -> BoolSetting(!pre_process_input_file_keep) );
  ( "detect-spec-is-empty-keep-solution-file"   , fun () -> BoolSetting(!pre_process_output_file_keep));

  ( "solver-flat-constraint-file"               , fun () -> StringSetting(!constraint_solver_flat_input_file)    );
  ( "solver-flat-solution-file"                 , fun () -> StringSetting(!constraint_solver_flat_output_file)   );
  ( "solver-flat-keep-constraint-file"          , fun () -> BoolSetting(!constraint_solver_flat_input_file_keep) );
  ( "solver-flat-keep-solution-file"            , fun () -> BoolSetting(!constraint_solver_flat_output_file_keep));

  ( "solver-constraint-file"                    , fun () -> StringSetting(!constraint_solver_classic_input_file)    );
  ( "solver-solution-file"                      , fun () -> StringSetting(!constraint_solver_classic_output_file)   );
  ( "solver-keep-constraint-file"               , fun () -> BoolSetting(!constraint_solver_classic_input_file_keep) );
  ( "solver-keep-solution-file"                 , fun () -> BoolSetting(!constraint_solver_classic_output_file_keep));


(* 06. Configuration Generation *)

  ( "generate-bindings" , fun () -> ConfGenBindingsSetting(!configuration_generation_bindings));
  ( "generate-packages" , fun () -> ConfGenPackagesSetting(!configuration_generation_packages));


(* 07. Output Configuration *)

  ( "results"                           , fun () -> OutputFileSetting(!output_file)         );
  ( "append-repository-to-package-name" , fun () -> BoolSetting(!data_package_name_extended));


(* 08. Verbose Options *)

  ( "verbose-stage" , fun () -> BoolSetting(!verbose_stage));

(* settings *)

  ( "verbose-settings-check-warning"         , fun () -> BoolSetting(!verbose_settings_check_warning));
  ( "verbose-settings-check-error"           , fun () -> BoolSetting(!verbose_settings_check_error)  );
  ( "verbose-settings-check-setting-non-set" , fun () -> BoolSetting(!verbose_settings_non_set)      );

(* 08.1. inputs *)

  ( "verbose-input-warning"                 , fun () -> BoolSetting(!verbose_input_warning));
  ( "verbose-input-error"                   , fun () -> BoolSetting(!verbose_input_error)  );

  ( "print-input-universe"                  , fun () -> BoolSetting(!verbose_input_universe)             );
  ( "print-input-repositories"              , fun () -> BoolSetting(!verbose_input_repositories)         );
  ( "print-full-universe"                   , fun () -> BoolSetting(!verbose_input_universe_full)        );
  ( "print-initial-configuration"           , fun () -> BoolSetting(!verbose_input_initial_configuration));
  ( "print-specification"                   , fun () -> BoolSetting(!verbose_input_specification)        );
  ( "print-optimization-function"           , fun () -> BoolSetting(!verbose_input_optimization_function));

  ( "extra-log-universe-check"              , fun () -> BoolSetting(!verbose_input_universe_check)             );
  ( "extra-log-repositories-check"          , fun () -> BoolSetting(!verbose_input_repositories_check)         );
  ( "extra-log-initial-configuration-check" , fun () -> BoolSetting(!verbose_input_initial_configuration_check));
  ( "extra-log-specification-check"         , fun () -> BoolSetting(!verbose_input_specification_check)        );

(* 08.2. pre-processs *)

  ( "print-is-spec-well-formed"          , fun () -> BoolSetting(!verbose_spec_fw_detection)                       );
  ( "print-is-spec-empty"                , fun () -> BoolSetting(!verbose_spec_empty_detection)                    );
  ( "extra-log-is-spec-well-formed"      , fun () -> BoolSetting(!verbose_spec_fw_detection_activities)            );
  ( "extra-log-is-spec-empty"            , fun () -> BoolSetting(!verbose_spec_empty_detection_activities)         );

  ( "print-universe-graph"               , fun () -> BoolSetting(!verbose_universe_graph)                          );
  ( "print-has-component-types-loop"     , fun () -> BoolSetting(!verbose_universe_loop_detection)                 );
  ( "print-component-types-bounds"       , fun () -> BoolSetting(!verbose_universe_bound_detection)                );
  ( "extra-log-has-component-types-loop" , fun () -> BoolSetting(!verbose_universe_loop_detection_activities)      );
  ( "extra-log-component-types-bounds"   , fun () -> BoolSetting(!verbose_universe_bound_detection_activities)     );

  ( "print-packages-trimmed"             , fun () -> BoolSetting(!verbose_universe_package_trim_package)           );
  ( "print-component-types-trimmed"      , fun () -> BoolSetting(!verbose_universe_component_types_trim)           );
  ( "extra-log-packages-trimmed"         , fun () -> BoolSetting(!verbose_universe_package_trim_package_activities));
  ( "extra-log-component-types-trimmed"  , fun () -> BoolSetting(!verbose_universe_component_types_trim_activities));

(* 08.3. constraints *)

  ( "print-main-constraint"              , fun () -> BoolSetting(!verbose_constraint)                  );
  ( "print-main-solution"                , fun () -> BoolSetting(!verbose_constraint_solver_activities));
  ( "extra-log-main-constraint-solving"  , fun () -> BoolSetting(!verbose_solution)                    );

(* 08.4. configuration generation *)

  ( "print-result-location+components"        , fun () -> BoolSetting(!verbose_gen_configuration_init)   );
  ( "print-result-location+components+pakage" , fun () -> BoolSetting(!verbose_gen_configuration_package));
  ( "print-result-full"                       , fun () -> BoolSetting(!verbose_gen_configuration_full)   )

]

let string_of_settings () =
  let settings_strings =
    List.map (fun (setting_name, get_setting_function) ->
      let setting = get_setting_function () in
      Printf.sprintf "%-45s = %s\n%!" setting_name (string_of_setting setting)
    ) settings_printing_functions
  in
  String.concat "" settings_strings
  
  *)
