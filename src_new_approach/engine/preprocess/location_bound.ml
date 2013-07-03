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


let constraint_of (resources, universe, config, spec) ls = (* TODO: put in constraint_of with model_with_domain *)
  let (_, k_universe) = List.split (Constraint_of.universe resources ls universe) in
  let (_, k_config) = List.split (Constraint_of.locations resources (Data_model.Location_set_of_location_ids.convert config#get_location ls)) in
  let (_, k_spec) = List.split (Constraint_of.universe ls spec) in
  Data_constraint.NaryKonstraint () ((List.flatten k_universe) @ (List.flatten k_config) @ (List.flatten k_spec))


type bounds = { min : int; max : int }
type el = Data_model.Location_id_set.t * bounds
type t = { q : el Queue.t; mutable s : Data_model.Location_id_set_set.t; mutable continue : bool }

let bounds_create s = { min = 0; max = Data_model.Location_id_set.cardinal s }

let create categories = 
  let q = Queue.create () in
  Data_model.Location_id_set_set.iter (fun s -> Queue.add (s, bounds_create s) q) categories;
  { q = q; s = Data_model.Location_id_set_set.empty; continue = true }


let get_domain lb = Queue.fold (fun res (c, _) -> Data_model.Location_id_set.union res c) lb.q
  (Data_model.Location_id_set_set.fold (fun c res -> Data_model.Location_id_set.union res c)  lb.s Data_model.Location_id_set.empty)

let step solve model lb =
  let (c, b) = Queue.take lb.q in let n = (b.min + b.max) / 2 in
  let c' = Data_model.Location_id_set.sub_cardinal n c in
  let ls = Data_model.Location_id_set.union c' (get_domain lb) in
  let k = constraint_of model ls in
  match solve k (Data_constraint.Lexicographic []) with
    | None   -> if n = b.max then lb.continue <- false else Queue.add (c, { min = n + 1; max = b.max}) lb.q
    | Some _ -> if n = b.max then lb.s <- Data_model.Location_id_set.add c' lb.s else  Queue.add (c', { min = b.min; max = n}) lb.q
let rec all_steps solve model lb =
  if lb.continue && (not (Queue.is_empty lb.q)) then (step solve model lb; all_steps solve model lb)


let fit_categories solve model categories =
  let lb = create categories in
  all_steps solve model lb;
  if lb.continue then Some(lb.s)
  else None



