type cudf_reason =
    R_conflict of int * int
  | R_depends of int * Cudf_types.vpkglist
  | R_install of Cudf_types.vpkg

type version
type p = Cudf.package

type pool =
  { mutable size : int;
    packages : (string * version, p) Hashtbl.t;
    packages_by_name : (string, int list ref) Hashtbl.t;
    packages_by_num : (int, p) Hashtbl.t;
    num_of_package : (Cudf_types.pkgname * Cudf_types.version, int) Hashtbl.t;
    provided_packages :
      (string, (Cudf_types.version option * p) list ref) Hashtbl.t }

let new_pool () =
  { size = 0;
    packages = Hashtbl.create 101;
    packages_by_name = Hashtbl.create 101;
    packages_by_num = Hashtbl.create 101;
    num_of_package = Hashtbl.create 101;
    provided_packages = Hashtbl.create 101 }

(****)

let pp_loc ch (start_pos, end_pos) =
  let line { Lexing.pos_lnum = l } = l in
  if line start_pos = line end_pos
  then Format.fprintf ch "line %d, char %d-%d" (line start_pos) start_pos.Lexing.pos_cnum end_pos.Lexing.pos_cnum
  else Format.fprintf ch "lines: %d-%d" (line start_pos) (line end_pos)

let load ic =
  try
    let p = Cudf_parser.from_in_channel ic in
    let infos = Cudf_parser.load p in
    Cudf_parser.close p;
    infos
  with Cudf_parser.Parse_error (s, l) as e ->
    Format.eprintf "%a: %s@." pp_loc l s;
    raise e

let pkgid p = (p.Cudf.package, p.Cudf.version)
let veqpkg p = (p.Cudf.package, Some (`Eq, p.Cudf.version))

(****)

let get_package_list' h n =
  try
    Hashtbl.find h n
  with Not_found ->
    let r = ref [] in
    Hashtbl.add h n r;
    r

let add_to_package_list h n p =
  let l = get_package_list' h n in
  l := p :: !l

let get_package_list h n = try !(Hashtbl.find h n) with Not_found -> []

(****)

let print_pack pool ch p =
  Cudf_types_pp.pp_veqpkg ch (veqpkg (Hashtbl.find pool.packages_by_num p))

let print_pack_name pool ch p =
  Cudf_types_pp.pp_pkgname ch (Hashtbl.find pool.packages_by_num p).Cudf.package

(****)

let rec remove_duplicates_rec x (l : int list) =
  match l with
    []     ->
      [x]
  | y :: r ->
      if x = y then
        remove_duplicates_rec x r
      else
        x :: remove_duplicates_rec y r

let remove_duplicates l =
  match l with
    []     -> []
  | x :: r -> remove_duplicates_rec x r

let normalize_set (l : int list) =
  remove_duplicates (List.sort (fun x y -> compare x y) l)

(****)

let print_rules = ref false

module Solver = Solver.F (struct type reason = cudf_reason end)
type reason = cudf_reason

let lookup_packages ?(filter=None) dist pkgname =
  let packages = get_package_list dist.provided_packages pkgname in
  if filter = None then packages else
  List.filter
    (fun (v, _) ->
       match v with
         None   -> true
       | Some v -> Cudf.version_matches v filter)
    packages

let resolve_package_dep dist (p, v) =
  List.map (fun (_, p) -> Hashtbl.find dist.num_of_package (pkgid p))
    (lookup_packages ~filter:v dist p)

(*
(*XXX*)
let rec print_package_disj ch l =
  match l with
    []     -> ()
  | [p]    -> Cudf_types_pp.pp_vpkg ch p
  | p :: r -> Cudf_types_pp.pp_vpkg ch p; Format.fprintf ch " | ";
              print_package_disj ch r
let rec print_package_list_rec print_var ch l =
  match l with
    []     -> Format.fprintf ch "NOT AVAILABLE"
  | [x]    -> print_var ch x
  | x :: r -> Format.fprintf ch "%a, %a"
                print_var x (print_package_list_rec print_var) r
let print_package_list pool ch l =
  Format.fprintf ch "{%a}" (print_package_list_rec pool) l
let resolve_package_dep dist p =
let res = resolve_package_dep dist p in
Format.eprintf "%a == %a@." Cudf_types_pp.pp_vpkg p (print_package_list (print_pack dist)) res;
res
*)

let add_conflict st l =
  let l = normalize_set l in
  if List.length l > 1 then begin
    if !print_rules then begin
      Format.printf "conflict (";
      List.iter (fun c -> Format.printf " %d" c) l;
      Format.printf ")@."
    end;
    let a = Array.of_list l in
    let len = Array.length a in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        let p = Solver.lit_of_var a.(i) false in
        let p' = Solver.lit_of_var a.(j) false in
        Solver.add_rule st [|p; p'|] [R_conflict (a.(i), a.(j))]
      done
    done
  end

let add_depend st deps n l =
  let l = normalize_set l in
  (* Some packages depend on themselves... *)
  if not (List.memq n l) then begin
    if !print_rules then begin
      Format.printf "%d -> any-of (" n;
      List.iter (fun c -> Format.printf " %d" c) l;
      Format.printf ")@."
    end;
    Solver.add_rule st
      (Array.of_list
         (Solver.lit_of_var n false ::
          List.map (fun n' -> Solver.lit_of_var n' true) l))
      [R_depends (n, deps)]
  end

let generate_rules pool =
  let pr = Solver.initialize_problem ~print_var:(print_pack pool) pool.size in
  Hashtbl.iter
    (fun i p ->
       if !print_rules then
         Format.eprintf "%a@." (print_pack pool) i;
       (* Dependences *)
       List.iter
         (fun l ->
            add_depend pr l i
              (List.flatten
                 (List.map (fun p -> resolve_package_dep pool p) l)))
         p.Cudf.depends;
       (* Conflicts *)
       List.iter
         (fun n -> add_conflict pr [i; n])
         (normalize_set
             (List.flatten
                (List.map (fun p -> resolve_package_dep pool p)
                   p.Cudf.conflicts))))
    pool.packages_by_num;
  Solver.propagate pr;
  pr

(****)

let all_packages = ref true

let parse_packages pool ignored_packages ch =
  if pool.size <> 0 then invalid_arg "Cudf_lib.parse_packages";
  let st = Common.start_parsing true ch in
  let (preamble, pkgs, req) = load ch in
  let i = ref 0 in
  let versions = Hashtbl.create 107 in
  Cudf.iter_packages
    (fun p ->
       try
         let v = Hashtbl.find versions p.Cudf.package in
         if v < p.Cudf.version then
           Hashtbl.replace versions p.Cudf.package p.Cudf.version
       with Not_found ->
         Hashtbl.replace versions p.Cudf.package p.Cudf.version)
    pkgs;

  Cudf.iter_packages
    (fun p ->
if
  !all_packages || p.Cudf.version = Hashtbl.find versions p.Cudf.package
then begin
  Common.parsing_tick st;
       Hashtbl.add pool.packages_by_num !i p;
       let id = pkgid p in
       assert (not (Hashtbl.mem pool.num_of_package id));
       Hashtbl.add pool.num_of_package id !i;
       add_to_package_list pool.packages_by_name p.Cudf.package !i;
       add_to_package_list pool.provided_packages p.Cudf.package
         (Some p.Cudf.version, p);
       List.iter
         (fun (q, v) ->
            let v = match v with Some (_, v) -> Some v | None -> None in
            add_to_package_list pool.provided_packages q (v, p))
         p.Cudf.provides;
       incr i
end)
    pkgs;
  Common.stop_parsing st;
  pool.size <- !i


(****)

let package_re = Str.regexp "^\\([^ (]+\\) *( *\\([<=>]+\\) *\\([0-9]+\\) *)$"

let parse_package_dependency pool s =
  if not (Str.string_match package_re s 0) then
    failwith (Format.sprintf "Bad package name '%s'" s);
  let name = Str.matched_group 1 s in
  let ver =
    try
      let rel =
        match Str.matched_group 2 s with
          "<<"       -> `Lt
        | "<=" | "<" -> `Leq
        | "="        -> `Eq
        | ">=" | ">" -> `Geq
        | ">>"       -> `Gt
        | s          -> failwith (Format.sprintf "Bad relation '%s'" s)
      in
      Some (rel, int_of_string (Str.matched_group 3 s))
    with Not_found ->
      None
  in
  resolve_package_dep pool (name, ver)

let parse_package_name pool s = get_package_list pool.packages_by_name s

(****)

let rec print_package_disj ch l =
  match l with
    []     -> ()
  | [p]    -> Cudf_types_pp.pp_vpkg ch p
  | p :: r -> Cudf_types_pp.pp_vpkg ch p; Format.fprintf ch " | ";
              print_package_disj ch r

let rec print_package_list_rec print_var ch l =
  match l with
    []     -> Format.fprintf ch "NOT AVAILABLE"
  | [x]    -> print_var ch x
  | x :: r -> Format.fprintf ch "%a, %a"
                print_var x (print_package_list_rec print_var) r

let print_package_list pool ch l =
  Format.fprintf ch "{%a}" (print_package_list_rec pool) l

let show_reasons pool l =
  if l <> [] then begin
    Format.printf "The following constraints cannot be satisfied:@.";
    List.iter
      (fun r ->
         match r with
           R_conflict (n1, n2) ->
             Format.printf "  %a conflicts with %a@."
               (print_pack pool) n1 (print_pack pool) n2
         | R_depends (n, l) ->
             Format.printf "  %a depends on %a %a@."
               (print_pack pool) n print_package_disj l
               (print_package_list (print_pack pool))
               (List.flatten (List.map (resolve_package_dep pool) l))
         | R_install p ->
             Format.printf "  need to install %a %a@."
               Cudf_types_pp.pp_vpkg p
               (print_package_list (print_pack pool))
               (resolve_package_dep pool p))

      l
  end

let conflicts_in_reasons rl = List.fold_left (fun cl -> function R_conflict (i,j) -> (min i j, max i j)::cl | _ -> cl) [] rl

(****)

let compute_conflicts pool =
  let conflict_pairs = Hashtbl.create 1000 in
  let conflicts = Hashtbl.create 1000 in
  Hashtbl.iter
    (fun i p ->
       List.iter
         (fun n ->
            let pair = (min n i, max n i) in
            if n <> i && not (Hashtbl.mem conflict_pairs pair) then begin
              Hashtbl.add conflict_pairs pair ();
              add_to_package_list conflicts i n;
              add_to_package_list conflicts n i
            end)
         (normalize_set
            (List.flatten
               (List.map (fun p -> resolve_package_dep pool p)
                   p.Cudf.conflicts))))
    pool.packages_by_num;
  Array.init pool.size (fun i -> get_package_list conflicts i)

let compute_deps dist =
  Array.init dist.size (fun i ->
    let p = Hashtbl.find dist.packages_by_num i in
    List.map
      (fun l ->
         normalize_set
           (List.flatten
              (List.map (fun p -> resolve_package_dep dist p)
                 l)))
      p.Cudf.depends)

(****)

let pool_size p = p.size
