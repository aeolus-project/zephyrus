(*
./check_coinstall -ignore daemontools-run /var/lib/apt/lists/ftp.fr.debian.org_debian_dists_testing_main_binary-amd64_Packages -ignore liboss-salsa-asound2 -ignore libgd2-noxpm -ignore libqt4-phonon -ignore libjack-jackd2-0 -ignore libjpeg8-dev -ignore libhdf4-dev -ignore libgl1-mesa-swx11
*)

let mark_all = ref false
let explain = ref false
let roots = ref []
let stats = ref false
let graph = ref "graph.dot"

(****)

let insert tbl x v =
  let l =
    try Hashtbl.find tbl x with Not_found ->
    let l = ref [] in Hashtbl.add tbl x l; l
  in
  l := v :: !l

let get tbl x = try !(Hashtbl.find tbl x) with Not_found -> []

(****)

module F (M : Api.S) = struct

  module Repository = Repository.F(M)
  open Repository
  module Quotient = Quotient.F (Repository)
  module Graph = Graph.F (Repository)

(****)

let simplify_formula confl f =
  Formula.filter
    (fun d ->
       Disj.for_all
         (fun p ->
            Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d)
    f

let filter_conflicts confl p f =
  Formula.map
    (fun d -> Disj.filter (fun q -> not (Conflict.check confl p q)) d) f

let filter_conflicts confl p f =
  Formula.fold
    (fun d nf ->
       Formula.conj nf
         (Formula.of_disj
            (Disj.filter
               (fun q ->
                  not (PSet.exists (fun r -> Formula.implies1 f (Disj.lit r))
                         (Conflict.of_package confl q)))
               d)))
    f Formula._true

let rec flatten_deps tbl dist deps conflicts visited l =
  Formula.fold
    (fun d (l, r) ->
       let (l', r') =
         Disj.fold
           (fun i (l, r) ->
              let (l', r') = flatten_dep tbl dist deps conflicts visited i in
              (Formula.disj l' l, PSet.union r r')) d (Formula._false, r)
       in
       (Formula.conj l' l, r'))
    l (Formula._true, PSet.empty)

and flatten_dep tbl dist deps conflicts visited i =
  try
    (Hashtbl.find tbl i, PSet.empty)
  with Not_found ->
    let res =
      if List.mem i visited then
        (Formula._true, PSet.singleton i)
      else begin
        let (l, r) =
          flatten_deps tbl dist deps conflicts (i :: visited) (PTbl.get deps i)
        in
        let l = simplify_formula conflicts l in
(*???
        let l = filter_conflicts conflicts i l in
*)
        let r = PSet.remove i r in
        if Conflict.has conflicts i then
          (Formula.conj (Formula.lit i) l, r)
        else
          (l, r)
      end
    in
    (* Only cache the result if it is unconditionally true *)
    if PSet.is_empty (snd res) then Hashtbl.add tbl i (fst res);
    res

let flatten_dependencies dist deps confl =
  let tbl = Hashtbl.create 17 in
  PTbl.init dist (fun p -> fst (flatten_dep tbl dist deps confl [] p))

(****)

let focus pool deps confl =
(*
- Map: i |-> (p, d)
- 
*)
let large = false in
  if !roots <> [] then begin
    let i = ref (-1) in
    let pieces = Hashtbl.create 101 in
    let piece_conflicts = PTbl.create pool [] in
    let package_pieces = PTbl.create pool [] in
    let tbl_add tbl p v = PTbl.set tbl p (v :: PTbl.get tbl p) in
    PTbl.iteri
      (fun p f ->
         Formula.iter f
           (fun d ->
              incr i;
              Disj.iter d
                (fun q -> tbl_add piece_conflicts q !i);
              tbl_add package_pieces p !i;
              Hashtbl.add pieces !i (p, d)))
      deps;
    let roots =
      List.fold_left
        (fun s nm ->
           List.fold_left (fun s p -> PSet.add (Package.of_index p) s)
             s (M.parse_package_name pool nm))
        PSet.empty !roots
    in
Format.eprintf "ROOTS: %d@." (PSet.cardinal roots);
let dp = deps in

    let new_deps = PTbl.create pool Formula._true in
    let new_confl = Conflict.create pool in
    let visited = ref PSet.empty in
    let packages = ref PSet.empty in
    let rec add_conflict p =
Format.eprintf "ADD confl: %a@." (Package.print pool) p;
      if not (PSet.mem p !visited) then begin
        visited := PSet.add p !visited;
        packages := PSet.add p !packages;
        PTbl.set new_deps p
          (Formula.conj (PTbl.get new_deps p) (Formula.lit p));
        PSet.iter
          (fun q ->
Format.eprintf "ADD confl: %a %a@." (Package.print pool) p (Package.print pool) q;
             Conflict.add new_confl p q;
             let l = PTbl.get piece_conflicts q in
             packages := PSet.add q !packages;
             List.iter (fun i -> add_piece (Some q) i) l)
          (Conflict.of_package confl p)
      end
    and add_piece q i =
      if large then begin
        let (p, d) = Hashtbl.find pieces i in
        let l = PTbl.get package_pieces p in
        List.iter (fun i -> add_piece_2 q i) l
      end else
        add_piece_2 q i

    and add_piece_2 q i =
      let (p, d) = Hashtbl.find pieces i in

Format.eprintf "ADD piece: %a => %a@." (Package.print pool) p (Disj.print pool) d;
      if
(*
true
*)
        PSet.mem p roots ||
(*FIX: or any equivalent package... *)
        not (Disj.exists (fun r -> PSet.mem r roots) d
                ||
(*FIX: or any equivalent package... *)
             (Some p <> q &&
              PSet.exists (fun r -> Conflict.check confl r p) roots)
)
      then begin
        packages := PSet.add p !packages;
        PTbl.set new_deps p
          (Formula.conj (PTbl.get new_deps p) (Formula.of_disj d));
        Disj.iter d (fun r -> if Some r <> q then add_conflict r)
      end
    and add_dep p =
      packages := PSet.add p !packages;
      let l = PTbl.get package_pieces p in
      List.iter
        (fun i -> add_piece None i)
        l
    in
    PSet.iter
      (fun p ->
Format.eprintf "ADD root: %a ==> %a@." (Package.print pool) p (Formula.print pool) (PTbl.get dp p);
         add_dep p;
(*
         PSet.iter add_dep (Conflict.of_package confl p)
*)
)
      roots;
    Conflict.iter confl
      (fun p q ->
         if
           PSet.mem p !packages &&
           Formula.implies (PTbl.get new_deps p) (Formula.lit p) &&
           PSet.mem q !packages &&
           Formula.implies (PTbl.get new_deps q) (Formula.lit q)
         then begin
           Conflict.add new_confl p q;
         end);
(*
*)
    (Some !packages, new_deps, new_confl)
  end else
    (None, deps, confl)

(****)

let read_data ignored_packages ic =
  let dist = M.new_pool () in
  M.parse_packages dist ignored_packages ic;
  let confl = Conflict.create dist in
  let c = M.compute_conflicts dist in
  Array.iteri
    (fun p1 l ->
       List.iter
         (fun p2 ->
            Conflict.add confl
              (Package.of_index p1) (Package.of_index p2))
         l)
    c;

  let deps =
    let d = M.compute_deps dist in
    PTbl.init dist
      (fun p ->
         Formula.conjl
           (List.map (fun l' -> Formula.lit_disj (Package.of_index_list l'))
           d.(Package.index p)))
  in
  (dist, deps, confl)

let print_problem quotient deps confl =
  let dist = Quotient.pool quotient in
  Quotient.iter
    (fun p ->
       let f = PTbl.get deps p in
       Format.eprintf "%a => %a@."
         (Package.print dist) p (Formula.print dist) f)
    quotient;
  Conflict.iter confl
    (fun p1 p2 ->
       Format.eprintf "%a ## %a@."
         (Package.print dist) p1 (Package.print dist) p2)

let generate_rules quotient deps confl =
  let dist = Quotient.pool quotient in
  let st =
    M.Solver.initialize_problem
      ~print_var:(M.print_pack dist) (M.pool_size dist) in
  Conflict.iter confl
    (fun p1 p2 ->
       let p1 = M.Solver.lit_of_var (Package.index p1) false in
       let p2 = M.Solver.lit_of_var (Package.index p2) false in
       M.Solver.add_rule st [|p1; p2|] []);
  Quotient.iter
    (fun p ->
       let f = PTbl.get deps p in
       Formula.iter f
         (fun d ->
            let l = Disj.to_lits d in
            if not (PSet.mem p l) then begin
              let l = List.map (fun p -> Package.index p) (PSet.elements l) in
              M.Solver.add_rule st
                (Array.of_list
                   (M.Solver.lit_of_var (Package.index p) false ::
                    List.map (fun p -> M.Solver.lit_of_var p true) l))
                [];
              match l with
                [] | [_] ->
                  ()
              | _ ->
                  M.Solver.associate_vars st
                    (M.Solver.lit_of_var (Package.index p) true) l
            end))
    quotient;
  st

(****)

let remove_redundant_conflicts dist deps confl =
  let conj_deps p =
    let f = PTbl.get deps p in
    Formula.fold
      (fun d s -> match Disj.to_lit d with Some p -> PSet.add p s | None -> s)
      f PSet.empty
  in
  Conflict.iter confl
    (fun p1 p2 ->
       let d1 = conj_deps p1 in
       let d2 = conj_deps p2 in
       if
         PSet.exists
           (fun q1 ->
              PSet.exists
                (fun q2 ->
                   (p1 <> q1 || p2 <> q2) &&
                   (p1 <> q2 || p2 <> q1) &&
                   Conflict.check confl q1 q2)
                d2)
           d1
       then begin
(*
         Format.eprintf "%a ## %a@."
           (Package.print dist) p1 (Package.print dist) p2;
*)
         Conflict.remove confl p1 p2
       end);
  let try_remove_conflict p1 p2 =
    let f1 = PTbl.get deps p1 in
    let d2 = conj_deps p2 in
    if
      Formula.exists
        (fun d1 ->
           Disj.for_all
             (fun q1 ->
                PSet.exists
                  (fun q2 ->
                     (p1 <> q1 || p2 <> q2) &&
                     (p1 <> q2 || p2 <> q1) &&
                     Conflict.check confl q1 q2)
                  d2)
             d1)
        f1
    then begin
(*
      Format.eprintf "%a ## %a@."
        (Package.print dist) p1 (Package.print dist) p2;
*)
      Conflict.remove confl p1 p2
    end
  in
  Conflict.iter confl try_remove_conflict;
  Conflict.iter confl (fun p1 p2 -> try_remove_conflict p2 p1);
  (* We may now be able to remove some dependencies *)
  PTbl.map (simplify_formula confl) deps

(****)

let remove_self_conflicts dist deps confl =
  let s = ref PSet.empty in
  PTbl.iteri
    (fun p f ->
       if Formula.exists (fun d -> match Disj.to_lit d with Some q -> Conflict.check confl p q | None -> false) f then
         s := PSet.add p !s)
    deps;
(*
PSet.iter (fun p -> Format.eprintf "SELF CONFLICT: %a@." (Package.print dist) p) !s;
*)
  PTbl.map
    (fun f ->
       Formula.fold
         (fun d f ->
            let d = Disj.filter (fun q -> not (PSet.mem q !s)) d in
            Formula.conj (Formula.of_disj d) f)
         f Formula._true)
    deps

(****)

module DepMap = Map.Make (Disj)
module DepSet = Set.Make (Disj)

let coinstallability_kernel quotient deps confl =
  let deps_of_confl = PTbl.create (Quotient.pool quotient) DepSet.empty in
  let all_deps = ref DepSet.empty in
  PTbl.iteri
    (fun p f ->
       Formula.iter f
         (fun d ->
            all_deps := DepSet.add d !all_deps;
            Disj.iter d
              (fun p ->
                 PTbl.set deps_of_confl p
                   (DepSet.add d (PTbl.get deps_of_confl p)))))
    deps;
  let forced_deps = ref DepSet.empty in
  let updated = ref false in
  let update_dep d =
    if not (DepSet.mem d !forced_deps) then begin
(*
Format.eprintf "Checking %a@." (Disj.print (Quotient.pool quotient)) d;
*)
      let c =
        Disj.fold
          (fun p n ->
            if n > 1 then n else begin
              if
                Conflict.for_all confl
                  (fun p' ->
(*
DepSet.iter (fun d ->
let pool = Quotient.pool quotient in
Format.eprintf "%a # %a => %a %b@." (Package.print pool) p (Package.print pool) p' (Disj.print pool) d (DepSet.mem d !forced_deps)) (PTbl.get deps_of_confl p');
*)
                     Disj.implies1 p' d
                       ||
                     DepSet.for_all
                       (fun d' ->
                          Disj.equiv d' d (*XXX implies?*)
                            ||
                          Disj.implies1 p d'
                            ||
                          DepSet.mem d' !forced_deps)
                       (PTbl.get deps_of_confl p')
(*
                     DepSet.subset (DepSet.remove d (PTbl.get deps_of_confl p')) !forced_deps
*)
)
                  p
              then
                n
              else
                1 + n
            end)
          d 0
      in
Format.eprintf "Checked (%d) %a@." c (Disj.print (Quotient.pool quotient)) d;
      if c <= 1 then begin
        updated := true;
Format.eprintf "Forced: %a@." (Disj.print (Quotient.pool quotient)) d;
        forced_deps := DepSet.add d !forced_deps
      end
    end
  in
prerr_endline "========================================";
  while
    updated := false;
    DepSet.iter update_dep !all_deps;
prerr_endline "========================================";
    !updated
  do () done;
  DepSet.iter
    (fun d ->
       if not (DepSet.mem d !forced_deps) then
       Format.eprintf "Not forced: %a@."
         (Disj.print (Quotient.pool quotient)) d)
    !all_deps;
  let deps' = PTbl.create (Quotient.pool quotient) Formula._true in
  Quotient.iter
    (fun p ->
       PTbl.set deps' p
         (Formula.filter
            (fun d -> not (DepSet.mem d !forced_deps))
            (PTbl.get deps p)))
    quotient;
  let targets = ref PSet.empty in
  DepSet.iter
    (fun d ->
       if not (DepSet.mem d !forced_deps) then
         Disj.iter d (fun p -> targets := PSet.add p !targets))
    !all_deps;
  let confl' = Conflict.create (Quotient.pool quotient) in
  Conflict.iter confl
    (fun p p' ->
      if PSet.mem p !targets || PSet.mem p' !targets then
        Conflict.add confl' p p');
  (deps', confl')

(****)

let array_mean a =
  let c = ref 0 in
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    let (u, v) = a.(i) in
    c := !c + u;
    s := !s + u * v
  done;
  float !s /. float !c

let array_median a =
  let c = ref 0 in
  for i = 0 to Array.length a - 1 do
    let (u, v) = a.(i) in
    c := !c + u
  done;
  let i = ref 0 in
  let s = ref 0 in
  while !s < (!c + 1) / 2 do
    let (u, v) = a.(!i) in
    s := !s + u;
    incr i
  done;
  snd a.(!i - 1)

let rec cone deps s p =
  if PSet.mem p s then s else
  let s = PSet.add p s in
  Formula.fold (fun d s -> Disj.fold (fun q s -> cone deps s q) d s)
    (PTbl.get deps p) s

let conflicts_count confl s =
  let count = ref 0 in
  PSet.iter (fun p -> if Conflict.has confl p then incr count) s;
  !count

let output_cone f pool deps confl p s =
(*
Format.eprintf "== %a ==@." (Package.print pool) p;
*)
  let quotient = Quotient.subset pool s in
  let cfl = Conflict.create pool in
  Conflict.iter
    confl
    (fun p q -> if PSet.mem p s && PSet.mem q s then Conflict.add cfl p q);
  Graph.output f ~mark_all:true
    ~package_weight:(fun p -> if Conflict.has confl p then 1000. else 1.)
    ~edge_color:(fun _ _ _ -> Some "blue") quotient deps cfl

let print_stats f txt quotient deps confl =
  let pkgs = ref 0 in
  let dps = ref 0 in
  let cones = ref [] in
  let confls = ref 0 in
  Quotient.iter
    (fun p ->
       incr pkgs;
       let c = cone deps PSet.empty p in
       let sz = PSet.cardinal c in
       let ncfl = conflicts_count confl c in
       if sz <= 38 && sz > 20 && ncfl = 3 then
         output_cone "/tmp/cone.dot" (Quotient.pool quotient) deps confl p c;
       cones :=
         (1 (*Quotient.class_size quotient p*),
          sz, ncfl) ::
         !cones;
       Formula.iter (PTbl.get deps p) (fun d -> if not (Disj.implies1 p d) then incr dps))
    quotient;
  Conflict.iter confl (fun _ _ -> incr confls);
  let cones = Array.of_list !cones in
  Array.sort (fun (_, x, _) (_, y, _) -> compare y x) cones;
  let ch = open_out (Format.sprintf "/tmp/%s.txt" f) in
  let s = ref 0 in
  for i = 0 to Array.length cones - 1 do
    let (w, sz, ncfl) = cones.(i) in
    Printf.fprintf ch "%d %d %d\n" !s sz ncfl;
    s := !s + w;
    if i = Array.length cones - 1 then
      Printf.fprintf ch "%d %d\n" !s sz
  done;
  close_out ch;
  let cones = Array.map (fun (w, sz, _) -> (w, sz)) cones in
  Format.eprintf "%s: %d package, %d dependencies, %d conflicts; cone size: %d / %f / %d@."
    txt !pkgs !dps !confls (snd cones.(0))
    (array_mean cones) (array_median cones)

(****)

let f ignored_packages ic =
  let (dist, deps, confl) = read_data ignored_packages ic in

  if !stats then
    print_stats "initial" "Initial repository"
      (Quotient.trivial dist) deps confl;

  let flatten_deps = flatten_dependencies dist deps confl in

(*XXX FIX: should iterate... *)
  let flatten_deps = remove_self_conflicts dist flatten_deps confl in

  let flatten_deps = remove_redundant_conflicts dist flatten_deps confl in

  let flatten_deps = flatten_dependencies dist flatten_deps confl in


  let maybe_remove fd2 p f d =
    Disj.exists (fun q ->
      Conflict.for_all confl (fun r ->
        Formula.exists (fun d' -> Disj.implies d' d && not (Disj.implies1 q d')) (PTbl.get fd2 r)) q
(*
&& (
Format.eprintf "%a =>(%a) %a@." (Package.print dist) p (Package.print dist) q (Disj.print dist) d;
true)
*)
) d
  in
  let is_composition fd2 p f d =
  Formula.exists (fun d' ->
    not (Disj.equiv d d') && not (Disj.equiv (Disj.lit p) d') &&
(
    let f =
      Disj.fold (fun p f -> Formula.disj (PTbl.get fd2 p) f) d' Formula._false
    in
    let res1 =
      Formula.exists (fun d'' -> Disj.implies d d'') f
    in
(*
    let res2 =
      not (Formula.implies (Formula.filter (fun d' -> not (Disj.equiv d d')) f) f)
    in
    let res =
    Disj.exists (fun q ->
      Formula.exists (fun d'' ->
        Disj.implies d (Disj.cut d' q d'')) (PTbl.get fd2 q)) d'
    in
if res1 <> res2 then begin
Format.eprintf "??? %b %b@." res1 res2
end;
*)
(*
if res <> res1 then begin
(*
Format.eprintf "%a : %a => %a@." (Package.print dist) p
(Disj.print dist) d (Formula.print dist) f;

Disj.iter d' (fun q ->
  if not (Formula.exists (fun d'' -> Disj.equiv (Disj.lit q) d'') (PTbl.get fd2 q)) then
    Format.eprintf "!!! %a => %a@." (Package.print dist) q (Formula.print dist) (PTbl.get fd2 q);
  Formula.iter (PTbl.get fd2 q) (fun d'' ->
     if Disj.implies d (Disj.cut d' q d'')
     then Format.eprintf "%a <= %a / %a / %a@." (Disj.print dist) d (Disj.print dist) d' (Package.print dist) q (Disj.print dist) d''
));
*)

Format.eprintf "!!! %b %b %b@." res res1 res2
end;
*)
res1
)
) f
in

(*
PTbl.iteri (fun p f ->
Formula.iter f (fun d -> if Conflict.exists confl (fun q -> Disj.implies1 q d) p then Format.eprintf "YYY %a ==> %a@." (Package.print dist) p (Disj.print dist) d
)) fd2;
*)

  let rec remove_deps deps =
    let changed = ref false in
    let deps =
      PTbl.mapi
        (fun p f ->
           Formula.filter (fun d ->
             let b =
               not (maybe_remove deps p f d) || is_composition deps p f d
             in
             if not b then changed := true;
             b) f)
        deps
    in
    if !changed then remove_deps deps else deps
  in

  let fd2 = remove_deps flatten_deps in

(*
  PTbl.iteri
    (fun p f ->
Formula.iter f (fun d ->
  if maybe_remove fd2 p f d then
Format.eprintf "REM %a: %a (%a)@." (Package.print dist) p (Disj.print dist) d (Formula.print dist) f)) fd2;
*)

(*??? Need to adapt other checks... (also, conflict to uninstallable package)
  let fd2 = PTbl.mapi (fun p f -> filter_conflicts confl p f) fd2 in
*)

(*XXXXX
  Build equivalence classes
  Focus up to equivalence
  Build equivalence classes on package in focus
*)
  let (domain, fd2, confl) = focus dist fd2 confl in

  (* Build package equivalence classes *)
  let quotient = Quotient.perform dist fd2 in
  let deps = Quotient.dependencies quotient fd2 in
  let confl = Quotient.conflicts quotient confl in
  Quotient.print quotient deps;

  (* Generate SAT problem *)
(*
  let st = M.generate_rules dist in
*)
(*XXX FIX: should consider *all* packages, not just the ones we focus on*)
  let st = generate_rules quotient deps confl in
(*
print_problem quotient fd2 confl;
*)
  let st' = if !explain then Some (M.generate_rules dist) else None in

  Util.title "NON-INSTALLABLE PACKAGES";
  let non_inst = ref PSet.empty in
  let is_installable i = not (PSet.mem i !non_inst) in
  Quotient.iter
    (fun p ->
       let i = Package.index p in
       if not (M.Solver.solve st i) then begin
         Format.printf "%a@." (Quotient.print_class quotient) p;
         begin match st' with
           Some st' ->
             PSet.iter
               (fun p ->
                  let i = Package.index p in
                  ignore (M.Solver.solve st' i);
                  M.show_reasons dist (M.Solver.collect_reasons st' i);
                  M.Solver.reset st')
               (Quotient.clss quotient p)
         | None ->
             ()
         end;
         non_inst := PSet.add p !non_inst
       end;
       M.Solver.reset st)
    quotient;
  Format.printf "@.";

  Util.title "NON-COINSTALLABLE PAIRS";
  let dep_tbl = Hashtbl.create 101 in
  let confl_tbl = Hashtbl.create 101 in
  Quotient.iter
    (fun p ->
       if is_installable p then begin
         let f = PTbl.get deps p in
         Formula.iter f
           (fun d ->
              Disj.iter d
                (fun q ->
                   if is_installable q then begin
                     insert dep_tbl q p;
                     PSet.iter
                       (fun r ->
                          if is_installable r then insert confl_tbl r p)
                       (Conflict.of_package confl q)
                   end))
       end)
    quotient;

  let pairs = Hashtbl.create 101 in
  let c = ref 0 in
  let c' = ref 0 in
  let c'' = ref 0 in
  let conflicts = Hashtbl.create 101 in
(*

  let coinstallable_pairs = ref PSetSet.empty in
  let non_coinstallable_pairs = ref PSetSet.empty in
*)
  Hashtbl.iter
    (fun p l ->
       let l' = get dep_tbl p in
       List.iter
         (fun p ->
            let i = Package.index p in
            List.iter
              (fun q ->
                 let j = Package.index q in
                 let pair = (min i j, max i j) in
                 if i <> j && not (Hashtbl.mem pairs pair) then begin
                   Hashtbl.add pairs pair ();
                   if M.Solver.solve_lst st [i; j] then begin
((*
                     coinstallable_pairs :=
                       PSetSet.add (PSet.add i (PSet.singleton j))
                         !coinstallable_pairs
 *))
                   end else begin
                     incr c'';
                     let r =
                       match st' with
                         Some st' ->
                           ignore (M.Solver.solve_lst st' [i; j]);
                           let r = M.Solver.collect_reasons_lst st' [i; j] in
                           M.Solver.reset st';
                           r
                       | None ->
                           []
                     in
                     insert conflicts p (q, r);
                     insert conflicts q (p, r);
((*
                     non_coinstallable_pairs :=
                       PSetSet.add (PSet.add i (PSet.singleton j))
                         !non_coinstallable_pairs
 *))
                   end;
                   M.Solver.reset st;
                   incr c'
                 end)
              l')
         !l;
       c := !c + List.length !l * List.length l')
    confl_tbl;
(*
  if !debug then Format.eprintf "Pairs: %d - %d - %d@." !c !c' !c'';
*)

  let cl = ref [] in
  Hashtbl.iter
    (fun i l ->
       let c = ref 0 in
       List.iter (fun (j, _) -> c := !c + Quotient.class_size quotient j) !l;
       cl := (!c, (i, !l)) :: !cl)
    conflicts;
  let sort l = List.sort (fun (c, _) (c', _) -> - compare c c') l in
  List.iter
    (fun (c, (i, l)) ->
       Format.printf "%d %a:" c (Quotient.print_class quotient) i;
       let l =
         sort (List.map
                 (fun (j, r) -> (Quotient.class_size quotient j, (j, r))) l)
       in
       let nf = ref false in
       List.iter
         (fun (c, (j, _)) ->
            if !nf then Format.printf ","; nf := true;
            Format.printf " %a" (Quotient.print_class quotient) j) l;
       Format.printf "@.";
       List.iter (fun (_, (_, r)) -> if r <> [] then M.show_reasons dist r) l)
    (sort !cl);
  let pw =
     List.fold_left
       (fun m (c, (p, _)) -> PMap.add p (max 1. (float c /. 4.)) m)
       PMap.empty !cl
  in

(******************
  let (deps, confl) = coinstallability_kernel quotient deps confl in
******************)

  let comp_count = ref 0 in
  let comps = Hashtbl.create 107 in
  Quotient.iter
    (fun p ->
       let f = PTbl.get deps p in
       Formula.iter f
         (fun d -> if is_composition deps p (PTbl.get deps p) d then begin
             incr comp_count;
             Hashtbl.add comps (p, d) ()
         end))
    quotient;
(*
  Format.eprintf "Comp count: %d@." !comp_count;
*)

  let comp_count = ref 0 in
  let rec remove_composition deps =
(*
prerr_endline "COMP";
*)
    let changed = ref false in
    Quotient.iter
      (fun p ->
         let found = ref false in
         PTbl.set deps p
           (Formula.filter
              (fun d ->
                 !found || begin
                   let b = is_composition deps p (PTbl.get deps p) d in
                   found := b;
                   if !found then begin changed := true; incr comp_count end;
                   not b
                 end)
              (PTbl.get deps p)))
      quotient;
    if !changed then remove_composition deps
  in
  remove_composition deps;
(*
  Format.eprintf "Comp count: %d@." !comp_count;
*)

  let comp_count = ref 0 in
(*  let comps = Hashtbl.create 107 in*)
  Quotient.iter
    (fun p ->
       let f = PTbl.get deps p in
       Formula.iter f
         (fun d -> if is_composition deps p (PTbl.get deps p) d then begin
             incr comp_count;
(*             Hashtbl.add comps (p, d) ()*)
         end))
    quotient;
(*
  Format.eprintf "Comp count: %d@." !comp_count;
*)

  let package_weight p = try PMap.find p pw with Not_found -> 1. in
  let edge_color p f d =
(*
    if (*maybe_remove deps p f d &&*) (*is_composition deps p f d*)
      Hashtbl.mem comps (p, d)
    then
      Some "violet"
    else
*)
      Some "blue"
  in

  if !stats then
    print_stats "final" "Final repository" quotient deps confl;

  Graph.output !graph ~mark_all:(!mark_all) ~package_weight ~edge_color
    quotient deps confl;

end

type kind = (*Cudf |*) Deb | Rpm

let _ =
let ignored_packages = ref [] in
let kind = ref Deb in
let file = ref None in
Arg.parse
  [(*
   "-cudf",
   Arg.Unit (fun () -> kind := Cudf),
   "  Parse CUDF files";
   *)
   "-rpm",
   Arg.Unit (fun () -> kind := Rpm),
   "  Parse hdlist.cz (RPM) files";
   "-deb",
   Arg.Unit (fun () -> kind := Deb),
   "  Parse (Debian) binary package control files (default)";
   "-o",
   Arg.String (fun f -> graph := f),
   "FILE   Output graph to file FILE";
   "-ignore",
   Arg.String (fun p -> ignored_packages := p :: !ignored_packages),
   "PACKAGE   Ignore package of name PACKAGE";
(*
   "-max",
   Arg.Int (fun n -> max_size := n),
   "N   Limit to the size of non-coinstallable sets searched (default: 2)";
   "-graph",
   Arg.String (fun f -> graph_file := Some f),
   "FILE   Output coinstallability graph to file FILE";
*)
   "-all",
   Arg.Unit (fun () -> mark_all := true),
   "  Include all packages in the coinstallability graph";
   "-root",
   Arg.String (fun p -> roots := p :: !roots),
   "  Draw only the relevant portion of the graph around this package";
   "-explain",
   Arg.Unit (fun () -> explain := true),
   " Explain the results";
   "-stats",
   Arg.Unit (fun () -> stats := true),
   " Output stats regarding the input and output repositories";
(*
   "-debug",
   Arg.Unit (fun () -> debug := true),
   "  Output debugging informations";
*)
  ]
  (fun p -> file := Some p)
  ("Usage: " ^ Sys.argv.(0) ^ " [OPTION]...\n\
    Analyze package coinstallability.  The package information are read\n\
    from the standard input.\n\
    \n\
    Options:");

let ic = match !file with None -> stdin | Some f -> open_in f in
match !kind with
(*  Cudf -> let module M = F(Cudf_lib) in M.f !ignored_packages ic*)
| Deb  -> let module M = F(Deb_lib) in M.f !ignored_packages ic
| Rpm  -> let module M = F(Rpm_lib) in M.f !ignored_packages ic
