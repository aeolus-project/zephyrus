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

(* Depends on nothing.
*)

module Linked_list : sig (* TODO: Why do we need another signature here when we already have the .mli file? *)
  type 'a t                                               (* type of a generic double linked list *)
  
  val create : unit -> 'a t                               (* create a new empty generic list *)
  val add_first : 'a -> 'a t -> unit                      (* add a new element at the beginning of the list *)
  val add_last  : 'a -> 'a t -> unit                      (* add a new element at the end of the list *)
  val add       : 'a -> 'a t -> unit                      (* synonym of add_last *)

  val first : 'a t -> 'a                                  (* returns the first element of the list, or raise Not_found *)
  val last  : 'a t -> 'a                                  (* returns the last  element of the list, or raise Not_found *)
  
  val rm_first : 'a t -> unit                             (* removes the first element of the list, or raise Not_found *)
  val rm_last  : 'a t -> unit                             (* removes the last  element of the list, or raise Not_found *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* fold over the list, starting with its first element *)
  val fold_left  : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b   (* fold over the list, starting with its last  element *)
  val fold       : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* synonym of fold_right *)

  val iter_right : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its first element *)
  val iter_left  : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its last  element *)
  val iter       : ('a -> unit) -> 'a t -> unit           (* synonym of iter_right *)

  val filter : ('a -> bool) -> 'a t -> 'a t               (* returns a new list keeping in the right order all the elements validating the input predicat *)

(*
  val prefix : 'a -> 'a t -> 'a t                         (* returns the smallest sub list that finishes with v in parameter, or raise Not_found *)
  val suffix : 'a -> 'a t -> 'a t                         (* returns the biggest sub list that starts with v in parameter, or raise Not_found *)
  val sub    : 'a -> 'a -> 'a t -> 'a t                   (* equivalent to prefix v2 (suffix v1 l) *)
*)

  val append : 'a t -> 'a t -> 'a t                       (* concat the two lists together *)
  val concat : 'a t list -> 'a t                          (* concat all the lists together *)

  val to_list : 'a t -> 'a list
  
  
  type 'a iterator
  
  val forward_iterator : 'a t -> 'a iterator
  val backward_iterator : 'a t -> 'a iterator
  
  val has_next : 'a iterator -> bool
  val next     : 'a iterator -> 'a
  
end = struct

  type 'a elt = { mutable pred : 'a elt option; value : 'a; mutable succ : 'a elt option}
  type 'a t = { mutable first : 'a elt option; mutable last : 'a elt option}

  let create () = { first = None; last = None }

  let add_first v l = let el = { pred = None; value = v; succ = l.first } in let sel = Some(el) in (match l.first with
    | None -> l.last <- sel
    | Some(el') -> el'.pred <- sel); l.first <- Some(el)

  let add_last v l =  let el = { pred = l.last; value = v; succ = None } in let sel = Some(el) in (match l.last with
    | None -> l.first <- sel
    | Some(el') -> el'.succ <- sel); l.last <- Some(el)

  let add = add_last

  let first_el l = match l.first with | None -> raise Not_found | Some(el) -> el
  let last_el  l = match l.last  with | None -> raise Not_found | Some(el) -> el
  let pred el = match el.pred  with | None -> raise Not_found | Some(el') -> el'
  let succ el = match el.succ  with | None -> raise Not_found | Some(el') -> el'

  let first l = (first_el l).value
  let last  l = (last_el  l).value

  let rm_first l = match l.first with
    | None -> raise Not_found
    | Some(el) -> l.first <- el.succ; match l.first with | None -> () | Some(el') -> el.pred <- None
  let rm_last  l = match l.last with
    | None -> raise Not_found
    | Some(el) -> l.last  <- el.pred; match l.last  with | None -> () | Some(el') -> el.succ <- None

  let fold_right f l accu = 
    let rec step el accu = let accu' = f el.value accu in match el.succ with None -> accu' | Some(el') -> step el' accu' in match l.first with
    | None -> accu | Some(el) -> step el accu
  let fold_left f accu l = 
    let rec step el accu = let accu' = f accu el.value in match el.pred with None -> accu' | Some(el') -> step el' accu' in match l.last with
    | None -> accu | Some(el) -> step el accu
  let fold = fold_right

  let iter_right f l =
    let rec step el = f el.value; match el.succ with None -> () | Some(el') -> step el' in match l.first with
    | None -> () | Some(el) -> step el
  let iter_left f l = 
    let rec step el = f el.value; match el.pred with None -> () | Some(el') -> step el' in match l.last with
    | None -> () | Some(el) -> step el
  let iter = iter_right

  let filter f l = fold_right (fun v res -> if f v then (add_last v res; res) else res) l (create ())

(*
  let rec find_el_el v el  = Printf.printf "node "; flush stdout; if el.value = v then (Printf.printf "found\n "; flush stdout; el) else match el.succ with | None -> raise Not_found | Some(el') -> Printf.printf "next "; flush stdout; find_el_el v el'
  let find_el v l = find_el_el v (first_el l)

  let extract first last =
    let first' = {pred = None; value = first.value; succ = None } in
    let rec step pred el = Printf.printf "toto "; flush stdout; let el' = { pred = Some(pred); value = el.value; succ = None } in
      pred.succ <- Some(el'); if el = last then el' else step el' (succ el) in
    let last' = if first = last then first' else step first' (succ first) in { first = Some(first'); last = Some(last') }

  let prefix v l = let el = find_el v l in extract (first_el l) el
  let suffix v l = let el = find_el v l in extract el (last_el l)
  let sub v1 v2 l = let el1 = find_el v1 l in let el2 = find_el_el v2 el1 in extract el1 el2
*)
  
  let append l1 l2 = let res = create () in
    iter_right (fun v -> add_last v res) l1;
    iter_right (fun v -> add_last v res) l2; res
    
  let rec concat l = match l with | [] -> create () | e::l' -> append e (concat l')
  
  
  let to_list l = fold_left (fun res v -> v::res) [] l
  
  type 'a iterator = { mutable element : 'a elt option; step : 'a elt -> 'a elt option }
  
  let forward_iterator  l = { element = l.first; step = fun el -> el.succ } 
  let backward_iterator l = { element = l.last; step = fun el -> el.pred } 

  let has_next i = i.element != None
  let next i     = match i.element with | None -> raise Not_found | Some(el) -> i.element <- i.step el; el.value

end