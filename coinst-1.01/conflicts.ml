(*
Data Reduction and Exact Algorithms for Clique Cover
Jens Gramm, Jiong Guo, Falk Huffner, and Rolf Niedermeier

XXX This implementation should be made more robust: at the moment, if we fail
to decompose the conflicts into obvious maximal cliques, we do not try to find
any other clique...
*)

module F (R : Repository.S) = struct

open R

module Quotient = Quotient.F(R)

module Pair (X : Set.OrderedType) (Y : Set.OrderedType) =
  struct
    type t = X.t * Y.t
    let compare (x, y) (x', y') =
      let c = X.compare x x' in if c = 0 then Y.compare y y' else c
  end

module PPairMap = Map.Make (Pair (Package) (Package))

let print_clique quotient s =
  Format.eprintf "Clique:";
  PSet.iter
    (fun p -> Format.eprintf " %a" (Quotient.print_class quotient) p) s;
  Format.eprintf "@."

let f quotient confl =
  let l = ref [] in
  let common = ref PPairMap.empty in
  Conflict.iter confl
    (fun p q ->
       let c =
         PSet.inter
           (Conflict.of_package confl p)
           (Conflict.of_package confl q)
       in
       let n = ref 0 in
       Conflict.iter confl
         (fun p' q' -> if PSet.mem p' c && PSet.mem q' c then incr n);
       assert (p < q);
       common := PPairMap.add (p, q) (c, PSet.cardinal c, !n) !common);
  let covered = Conflict.create (Quotient.pool quotient) in
  let removed = PTbl.create (Quotient.pool quotient) false in
  let changed = ref false in
  while
    changed := false;
(*
prerr_endline "AAAAA";
*)
    PPairMap.iter
      (fun (p, q) (c, i, n) ->
         let m = (i * (i - 1)) / 2 in
         if m = n && not (Conflict.check covered p q) then begin
(*
           Format.eprintf "Rule 2: %a # %a : %d -- %d / %d %b@."
             (Quotient.print_class quotient) p (Quotient.print_class quotient) q
             i m n (m = n);
*)
           let c = PSet.add p (PSet.add q c) in
(*
           print_clique quotient c;
*)
           l := c :: !l;
           changed := true;
           PSet.iter
             (fun p ->
                PSet.iter
                  (fun q ->
                     if p < q then begin
                       Conflict.add covered p q;
                       common := PPairMap.remove (p, q) !common
                     end)
                  c)
             c;
           PSet.iter
             (fun p ->
                let neigh = Conflict.of_package confl p in
                if
                  not (PTbl.get removed p) &&
                  PSet.for_all (fun q -> Conflict.check covered p q) neigh
                then begin
                  PTbl.set removed p true;
                  Conflict.iter confl
                    (fun p' q' ->
                       if PSet.mem p' neigh && PSet.mem q' neigh then begin
(*
  Format.eprintf "%a => %a %a@."
    (Quotient.print_class quotient) p  (Quotient.print_class quotient) p' (Quotient.print_class quotient) q';
*)
                         try
                         let (d, i, n) = PPairMap.find (p', q') !common in
(*
  Format.eprintf "-@.";
*)
  assert (PSet.mem p d);
                         let d = PSet.remove p d in
                         let n = n - PSet.cardinal (PSet.inter neigh d) in
(*
  Format.eprintf "%d@." i;
*)
                         assert (n >= 0);
                         if i = 0 then
                           common := PPairMap.remove (p', q') !common
                         else
                           common :=
                             PPairMap.add (p', q')
                               (d, i - 1, n) !common
                         with Not_found ->
                           ()
                       end)
                end)
             c
         end)
      !common;
    !changed
  do () done;
  PPairMap.iter
    (fun (p, q) (c, i, n) ->
       let m = (i * (i - 1)) / 2 in
       Format.eprintf "Remaining edge: %a # %a : %d -- %d / %d %b@."
         (Quotient.print_class quotient) p (Quotient.print_class quotient) q
         i m n (m = n);
       l := PSet.add p (PSet.singleton q) :: !l)
    !common;
  !l

end
