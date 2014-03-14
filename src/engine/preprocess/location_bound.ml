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


(* this file contains the algorithm that compute the minimal number of required locations per category to host the configuration specified by the user *)

(* this function simply computes the constraint corresponding to the input universe, configuration and specification *)
let constraint_of (universe, config, spec) ls_ids = (* TODO: put in constraint_of with model_with_domain *)
  let (_, k_universe) = List.split (Constraint_of.universe ls_ids universe config) in
  let (_, k_config)   = List.split (Constraint_of.locations universe#get_resource_ids ls_ids config#get_location) in
  let (_, k_spec)     = List.split (Constraint_of.specification ls_ids spec) in
  Data_constraint.NaryKonstraint((Data_constraint.And), ((List.flatten k_universe) @ (List.flatten k_config) @ k_spec))


type bounds = { min : int; max : int }                                                  (* used for dychotomic search *)
type el = Data_model.Location_id_set.t * bounds                                         (* subset of a category, with its bound *)
type t = { q : el Queue.t; mutable s : Location_categories.t; mutable continue : bool } (* q: the categories to process, s: the result of the computation, continue: true if we didn't find a fix point *)

let string_of_el_list el_list =
  let string_of_el el =
    let category, bounds = el in Printf.sprintf "(%d,%d)" bounds.min bounds.max in
  Printf.sprintf "el_list: %s" (String.concat "," (List.map string_of_el el_list))

(* create categories create the t structure corresponding to categories that serves as input to the fix point algorigthm *)
let bounds_create s = { min = 0; max = Data_model.Location_id_set.cardinal s }
let create categories = Location_categories.map_to_list (fun c -> (c,bounds_create c)) categories
(*  let q = Queue.create () in
  Location_categories.iter (fun s -> Queue.add (s, bounds_create s) q) categories;
  { q = q; s = Location_categories.empty; continue = true }*)

(* return the set of locations contained in the queue of a t structure *)
let get_domain lb = Queue.fold (fun res (c, _) -> Data_model.Location_id_set.union res c)
  (Location_categories.fold (fun c res -> Data_model.Location_id_set.union res c)  lb.s Data_model.Location_id_set.empty) lb.q



let step solve ((u,conf,s) as model) (c,b) = (* dycothomic function, *)
(*let compare l1 l2 = (conf#get_location l2)#cost - (conf#get_location l1)#cost in *) (* this line deleted by Kuba *)
  let n = (b.min + b.max) / 2 in
(*let c' = Data_model.Location_id_set.keep_best_elements n compare c in *) (* this line deleted by Kuba *)
  let c' = Data_model.Location_id_set.keep_elements n c in (* this line added by Kuba *)
  let k = constraint_of model c' in 
  Zephyrus_log.log_execution (Printf.sprintf "fit_categories step: bounds = [%d;%d], solving with n = %d\n" b.min b.max n); flush stdout;
  match solve [("constraint", k)] (Data_constraint.Lexicographic []) with
    | None   -> Zephyrus_log.log_execution "no solution\n"; (c, { min = n + 1; max = b.max})
(*        if n = b.max 
        then lb.continue <- false 
        else Queue.add (c, { min = n + 1; max = b.max}) lb.q *)
    | Some _ -> Zephyrus_log.log_execution "solution\n"; (c', { min = b.min; max = n})
(*        if n = b.max
        then lb.s <- Location_categories.add c' lb.s
        else Queue.add (c', { min = b.min; max = n}) lb.q*)

let rec all_steps_category solve model (c,b) =
  let (c',b') = step solve model (c,b) in
  (* if (b'.max < b'.min) or (b.max = b'.max) then c' *) (* this line deleted by Kuba *)
  if (b'.max <= b'.min) then c' (* this line added by Kuba *)
  else all_steps_category solve model (c',b')

let rec all_steps solve model lb = match lb with
 | []      -> Location_categories.empty
 | el::lb' -> let cs = all_steps solve model lb' in 
              Location_categories.add (all_steps_category solve model el) cs
(*  if lb.continue && (not (Queue.is_empty lb.q)) then (step solve model lb; all_steps solve model lb)*)


let fit_categories solve model categories =
  let lb = create categories in
  Zephyrus_log.log_execution (Printf.sprintf "%s\n" (string_of_el_list lb));
  Some(all_steps solve model lb)



