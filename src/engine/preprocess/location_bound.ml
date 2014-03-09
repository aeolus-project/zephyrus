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


let constraint_of (universe, config, spec) ls_ids = (* TODO: put in constraint_of with model_with_domain *)
  let (_, k_universe) = List.split (Constraint_of.universe ls_ids universe) in
  let (_, k_config)   = List.split (Constraint_of.locations universe#get_resource_ids ls_ids config#get_location) in
  let (_, k_spec)     = List.split (Constraint_of.specification ls_ids spec) in
  Data_constraint.NaryKonstraint((Data_constraint.And), ((List.flatten k_universe) @ (List.flatten k_config) @ k_spec))


type bounds = { min : int; max : int }
type el = Data_model.Location_id_set.t * bounds
type t = { q : el Queue.t; mutable s : Location_categories.t; mutable continue : bool }

let bounds_create s = { min = 0; max = Data_model.Location_id_set.cardinal s }

let create categories = 
  let q = Queue.create () in
  Location_categories.iter (fun s -> Queue.add (s, bounds_create s) q) categories;
  { q = q; s = Location_categories.empty; continue = true }


let get_domain lb = Queue.fold (fun res (c, _) -> Data_model.Location_id_set.union res c)
  (Location_categories.fold (fun c res -> Data_model.Location_id_set.union res c)  lb.s Data_model.Location_id_set.empty) lb.q

let step solve model lb =
  let (c, b) = Queue.take lb.q in let n = (b.min + b.max) / 2 in
  let c' = Data_model.Location_id_set.keep_elements n c in
  let ls = Data_model.Location_id_set.union c' (get_domain lb) in
  let k = constraint_of model ls in 
  Zephyrus_log.log_execution (Printf.sprintf "solving with n = %d\n" n); flush stdout;
  match solve [("constraint", k)] (Data_constraint.Lexicographic []) with
    | None   -> 
        Zephyrus_log.log_execution "no solution\n"; 
        if n = b.max 
        then lb.continue <- false 
        else Queue.add (c, { min = n + 1; max = b.max}) lb.q
    | Some _ -> 
        Zephyrus_log.log_execution "solution\n";
        if n = b.max
        then lb.s <- Location_categories.add c' lb.s
        else Queue.add (c', { min = b.min; max = n}) lb.q
let rec all_steps solve model lb =
  if lb.continue && (not (Queue.is_empty lb.q)) then (step solve model lb; all_steps solve model lb)


let fit_categories solve model categories =
  let lb = create categories in
  all_steps solve model lb;
  if lb.continue then Some(lb.s)
  else None



